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

	public CompilerOutputItem(final String fileName, final int lineNumber,
			final String comment) {
		fFileName = fileName;
		fLineNumber = lineNumber;
		fComment = comment.trim();
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

	public String getComment() {
		return fComment;
	}

	public void populateMarker(IMarker marker) throws CoreException {
        marker.setAttribute(IMarker.MESSAGE, fComment);
        marker.setAttribute(IMarker.LINE_NUMBER, fLineNumber);
        marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
	}
}