// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de>
package net.sf.eclipsefp.haskell.core.compiler;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;

/**
 * <p>
 * Default implementation for the information that was parsed out of the
 * compiler output.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class CompilerOutputItem implements ICompilerOutputItem {

	private String fFileName;

	private int fLineNumber;

	private String fComment;

	private int fStartColumn;

	private int fEndColumn;

	public CompilerOutputItem() {
		//placeholder constructor
	}

	public CompilerOutputItem(final String fileName, final int lineNumber,
			final String comment) {
		fFileName = fileName;
		fLineNumber = lineNumber;
		fComment = comment.trim();
	}

	public CompilerOutputItem(String fileName, int lineNumber, int startColumn,
		                      int endColumn, String comment)
	{
		fFileName = fileName;
		fLineNumber = lineNumber;
		fStartColumn = startColumn;
		fEndColumn = endColumn;
		fComment = comment;
	}


	public void addToComment(final String commentAddition) {
		String start = (fComment.equals("")) ? fComment : fComment + "\n";
		this.fComment = start + commentAddition.trim();
	}

	public String toString() {
		return "CompilerOutputItem:" + "\n  file   : " + fFileName
				+ "\n  line   : " + fLineNumber + "\n  comment: " + fComment;
	}

	// attribute setters and getters
	// //////////////////////////////

	public int getLine() {
		return fLineNumber;
	}

	public int getStartColumn() {
		return fStartColumn;
	}

	public int getEndColumn() {
		return fEndColumn;
	}

	public String getComment() {
		return fComment;
	}

	public void populateMarker(IMarker marker) throws CoreException {
        marker.setAttribute(IMarker.MESSAGE, fComment);
        marker.setAttribute(IMarker.LINE_NUMBER, fLineNumber);
        marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
        marker.setAttribute(IMarker.CHAR_START, fStartColumn);
        marker.setAttribute(IMarker.CHAR_END, fEndColumn);
	}

	public void setFileName(String fileName) {
		fFileName = fileName;
	}

	public void setLine(int line) {
		fLineNumber = line;
	}

	public void setStartColumn(int column) {
		fStartColumn = column;
	}

	public void setEndColumn(int column) {
		fEndColumn = column;
	}

	public void setComment(String comment) {
		fComment = comment;
	}

}