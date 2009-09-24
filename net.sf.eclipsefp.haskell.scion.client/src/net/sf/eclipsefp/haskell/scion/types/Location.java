package net.sf.eclipsefp.haskell.scion.types;

import org.eclipse.core.resources.IFile;
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

	public Location(IFile f,JSONObject json) throws JSONException {
		this.fileName = json.optString("file");
		this.otherName=json.optString("other");
		if ((this.fileName==null || this.fileName=="")&& (this.otherName==null || this.otherName=="")&& f!=null){
			this.fileName=f.getLocation().toOSString();
		} 
		JSONArray region = json.getJSONArray("region");
		startLine = region.getInt(0) - 1;
		startColumn = region.getInt(1);
		endLine = region.getInt(2) - 1;
		endColumn = region.getInt(3);
	}
	
	public Location(String fileName, int startLine, int startColumn, int endLine, int endColumn) {
		this.fileName = fileName;
		this.startLine = startLine;
		this.startColumn = startColumn;
		this.endLine = endLine;
		this.endColumn = endColumn;
	}
	
	public Location(String fileName, IDocument document, IRegion region) throws BadLocationException {
		this.fileName = fileName;
		int startOffset = region.getOffset();
		int endOffset = startOffset + region.getLength();
		this.startLine = document.getLineOfOffset(startOffset);
		this.startColumn = startOffset - document.getLineOffset(startLine);
		this.endLine = document.getLineOfOffset(endOffset);
		this.endColumn = endOffset - document.getLineOffset(endLine);
	}
	
	/**
	 * Returns the offset within the given document
	 * that the start of this {@link Location} object represents.
	 */
	public int getStartOffset(IDocument document) throws BadLocationException {
		return document.getLineOffset(startLine) + startColumn;
	}

	/**
	 * Returns the offset within the given document
	 * that the end of this {@link Location} object represents.
	 */
	public int getEndOffset(IDocument document) throws BadLocationException {
		return document.getLineOffset(endLine) + endColumn;
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
	
}
