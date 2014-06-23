package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultLineTracker;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ILineTracker;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.texteditor.MarkerUtilities;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A span of text within a file, in line:column format.
 * Lines are one-based, columns are zero-based.
 * The start is inclusive; the end is exclusive.
 * 
 * @author Thomas ten Cate
 */
public class Location {
	private String fileName;
	private String otherName;

	private int startLine, startColumn, endLine, endColumn;
	
	public Location(JSONObject json) throws JSONException {
		this(null,json);
	}

	public Location(IFile f, JSONArray json) throws JSONException {
		this(f!=null?f.getLocation().toOSString():"",json);
	}
	
	public Location(String fn, JSONArray json) throws JSONException {
		startLine=json.getInt(0);
		startColumn=json.getInt(1)-1; // we're zero based, Haskell code 1 based
		if (json.length()>3){
			endLine=json.getInt(2);
			endColumn=json.getInt(3)-1;// we're zero based, Haskell code 1 based
		} else if (json.length()>2){
			endLine=startLine;
			endColumn=json.getInt(2)-1;// we're zero based, Haskell code 1 based
		} else {
			endLine=startLine;
			endColumn=startColumn+1;
		}
		if (endColumn==-1 && endLine>startLine){
			endLine--;
		}
		this.fileName = fn;
	}
	
	public Location(IFile f, JSONObject json) throws JSONException {
		this.fileName  = json.optString("file");
		this.otherName = json.optString("other");
		
		if (   f != null
		    && (this.fileName == null || this.fileName.length() == 0)
		    && (this.otherName == null || this.otherName.length() == 0) ) {
		  // Default the file name to the Java file resource
		  this.fileName = f.getLocation().toOSString();
		} 
		
		if (json.optString("no-location").length() == 0) {
			JSONArray region = json.getJSONArray("region");
			startLine = region.getInt(0);
			startColumn = region.getInt(1);
			if (startColumn<0){
				startColumn=0;
			}
			endLine = region.getInt(2);
			endColumn = region.getInt(3);
		}
	}
	
	/**
	 * clone
	 * @param l
	 */
	public Location(Location l) {
		this.fileName = l.getFileName();
		this.startLine = l.getStartLine();
		this.startColumn = l.getStartColumn();
		
		this.endLine = l.getEndLine();
		this.endColumn = l.getEndColumn();
	}
	
	public Location(String fileName, int startLine, int startColumn, int endLine, int endColumn) {
		this.fileName = fileName;
		this.startLine = startLine;
		this.startColumn = startColumn;
		int mv=0;
		if (startColumn<0){
			mv=0-startColumn;
			this.startColumn=0;
		}
		this.endLine = endLine;
		this.endColumn = endColumn+mv;
	}
	
	public Location(String fileName, IDocument document, IRegion region) throws BadLocationException {
		this.fileName = fileName;
		int startOffset = region.getOffset();
		int endOffset = startOffset + region.getLength();
		int docLine=document.getLineOfOffset(startOffset);
		this.startLine =docLine+1 ;
		this.startColumn = startOffset - document.getLineOffset(docLine);
		docLine=document.getLineOfOffset(endOffset);
		this.endLine = docLine+1;
		this.endColumn = endOffset - document.getLineOffset(docLine);
	}
	
	
		/**
		 * Create a location from a buildwrapper location, expanding empty spans when possible.
		 */
		public Location(IProject project, String fileName, int startLin, int startCol, int endLin, int endCol) {
			this.fileName = fileName;
			startLine = startLin;
			startColumn = startCol - 1; // Buildwrapper columns start at 1
			endLine = endLin;
			endColumn = endCol - 1;      // Buildwrapper columns start at 1
			
			if (endLine > startLine) { // IMarker does not support multi-line spans, so we reduce it to an empty span
				endLine = startLine;
				endColumn = startColumn;
			}
			if (startLine==endLine && startColumn==endColumn) { // span is empty
				// If the span is empty, we try to extend it to extend it to a single character span.
				ILineTracker lineTracker = getLineTracker(project.getLocation().toOSString() +"/"+ BWFacade.DIST_FOLDER+"/"+fileName);
				if (lineTracker != null) {
					try {
						if (startLine>lineTracker.getNumberOfLines()){
							startLine=lineTracker.getNumberOfLines();
						}
						if (endLine>lineTracker.getNumberOfLines()){
							endLine=lineTracker.getNumberOfLines();
						}
						
						//System.err.println("Initial span: "+startLine+":"+startColumn+" to "+endLine+":"+endColumn);
						String delimiter = lineTracker.getLineDelimiter(startLine-1); // apparently this can return null
						int lineLength = lineTracker.getLineLength(startLine-1 /*LineTracker is 0 based*/ )
								             - (delimiter == null ? 0 : delimiter.length()); // subtract the delimiter length 
						
							if (startColumn < lineLength) { // not past the last character, so we can extend to the right.
								endColumn += 1;
							} else {
								if (startColumn > 0) { // past last character, but there are characters to the left. 
									startColumn -= 1;
								} 
								// else, we have startColumn == lineLength == 0, so the line is empty and we cannot extend the span.
							}
						
						//System.err.println("Fixed span: "+startLine+":"+startColumn+" to "+endLine+":"+endColumn);
		
					} catch (BadLocationException e){
						BuildWrapperPlugin.logError(BWText.process_parse_note_error, e);
					}
				} else {
					//System.err.println("LineTracker is null for file "+fileName);
				}
			}
		}
	
		/**
		 * Create a LineTracker for the file at filePath, for easy querying line lengths. 
		 * If any exception occurs, we simply return null.
		 * Note that line numbers are 0 based, and the returned length includes the separator.
		 */
		private static ILineTracker getLineTracker(String filePath) {
			ILineTracker lineTracker;
	    try (InputStream input = new FileInputStream( filePath ) ){
	      lineTracker = new DefaultLineTracker();

	      byte[] contents = new byte[input.available()];
	      input.read(contents);
	      String stringContents = new String(contents);
	      lineTracker.set(stringContents);
	    }
	    catch(IOException e) {
	      lineTracker = null;
	    }
	    return lineTracker;
	  }

	
	/**
	 * Returns the offset within the given document
	 * that the start of this {@link Location} object represents.
	 */
	public int getStartOffset(IDocument document) throws BadLocationException {
		return document.getLineOffset(startLine-1) + startColumn;
	}

	/**
	 * Returns the offset within the given document
	 * that the end of this {@link Location} object represents.
	 */
	public int getEndOffset(IDocument document) throws BadLocationException {
		return document.getLineOffset(endLine-1) + endColumn;
	}
	
	public int getLength(IDocument document) throws BadLocationException {
		return getEndOffset(document) - getStartOffset(document);
	}
	
	public String getContents(IDocument document) throws BadLocationException {
		int st=getStartOffset(document);
		return document.get(st,getEndOffset(document)-st);
	}

	public String getFileName() {
		return fileName;
	}

	public String getOtherName() {
		return otherName;
	}
	
	public int getStartLine() {
		return startLine;
	}

	public int getStartColumn() {
		return startColumn;
	}

	public int getEndLine() {
		return endLine;
	}

	public int getEndColumn() {
		return endColumn;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Location)) {
			return false;
		}
		Location other = (Location)obj;
		return
			fileName.equals(other.fileName) &&
			startLine == other.startLine && startColumn == other.startColumn &&
			endLine == other.endLine && endColumn == other.endColumn;
	}
	
	@Override
	public int hashCode() {
		return fileName.hashCode()<<16+startLine<<8+startColumn;
	}
	
	@Override
	public String toString() {
		return String.format("%d:%d-%d:%d", startLine, startColumn, endLine, endColumn);
	}
	
	
	public IFile getIFile(IProject p){
		String pl=p.getLocation().toOSString();
		String loc=getFileName();
		if (loc!=null && loc.startsWith(pl)){
			return p.getFile(loc.substring(pl.length()));
		} else if (!new File(loc).isAbsolute()) {
			return p.getFile(loc);
		}
		return null;
	}

	public Map<Object,Object> getMarkerProperties(IDocument d){
		int line= Math.min(getStartLine(),d.getNumberOfLines());
		final Map<Object,Object> attributes=new HashMap<>();
		//MarkerUtilities.setLineNumber(attributes, line);
		//if (getStartLine()==getEndLine()){
		MarkerUtilities.setLineNumber(attributes, line);
		//}
			try {
				int offset=d.getLineOffset(line-1);
				int start=getStartColumn();
				MarkerUtilities.setCharStart(attributes, offset+start);
				offset=d.getLineOffset(Math.min(getEndLine(),d.getNumberOfLines())-1);
				int end=offset+getEndColumn();
				if (end>=d.getLength()){
					end=d.getLength()-1;
				}
				MarkerUtilities.setCharEnd(attributes, end);
				
			} catch (BadLocationException ble){
				// ignore
			}
		//}
		return attributes;
	}
	
	public Map<Object,Object> getMarkerProperties(int maxLines){
		int line= Math.min(getStartLine(),maxLines);
		final Map<Object,Object> attributes=new HashMap<>();
		MarkerUtilities.setLineNumber(attributes, line);
//		if (getStartLine()==getEndLine()){
//			int start=getStartColumn();
//		    int end=getEndColumn();
//			// if we have startColumn==endColumn we could take end+1
//			// BUT if end goes over the document size, or start is zero, or if Eclipse feels like it, the marker is not shown on the document
//			// so it's better to just show the line without more info 
//			if (end>start){
//				MarkerUtilities.setCharStart(attributes, start);
//				// exclusive
//				MarkerUtilities.setCharEnd(attributes, end-1);
//			}
//		}
		return attributes;
	}

	public void setStartLine(int startLine) {
		this.startLine = startLine;
	}

	public void setStartColumn(int startColumn) {
		this.startColumn = startColumn;
	}

	public void setEndLine(int endLine) {
		this.endLine = endLine;
	}

	public void setEndColumn(int endColumn) {
		this.endColumn = endColumn;
	}

	public boolean contains(int line,int column){
		if( startLine<=line && endLine>=line){
			if (startLine==line){
				if (startColumn>column){
					return false;
				}
			}
			if (endLine==line){
				if (endColumn<column){
					return false;
				}
			}
			return true;
		}
		return false;
	}
}
