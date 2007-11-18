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

	public CompilerOutputItem(final String fileName, final int lineNumber, final int startColumn,
		                      final int endColumn, final String comment)
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

	@Override
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

	public void populateMarker(final IMarker marker) throws CoreException {
        marker.setAttribute(IMarker.MESSAGE, fComment);
        marker.setAttribute(IMarker.LINE_NUMBER, fLineNumber);
        marker.setAttribute(IMarker.SEVERITY, getSeverity());
        marker.setAttribute(IMarker.CHAR_START, fStartColumn);
        marker.setAttribute(IMarker.CHAR_END, fEndColumn);
	}

	private int getSeverity() {
		int result = IMarker.SEVERITY_ERROR;
		if( fComment != null && fComment.trim().startsWith( "Warning") ) {
			result = IMarker.SEVERITY_WARNING;
		}
		return result;
	}

	public void setFileName(final String fileName) {
		fFileName = fileName;
	}

	public void setLine(final int line) {
		fLineNumber = line;
	}

	public void setStartColumn(final int column) {
		fStartColumn = column;
	}

	public void setEndColumn(final int column) {
		fEndColumn = column;
	}

	public void setComment(final String comment) {
		fComment = comment;
	}

}