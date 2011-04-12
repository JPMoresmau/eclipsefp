package net.sf.eclipsefp.haskell.scion.types;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A span of text within a file, in line:column format.
 * Lines and columns are zero-based.
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
			return (IFile)p.getFile(loc.substring(pl.length()));
		}
		return null;
	}
}
