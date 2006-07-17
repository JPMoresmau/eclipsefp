// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;

/**
 * <p>
 * Represents what the compiler had to say about one particular problem at a
 * line in a file. These items are used for displaying compiler outputs in
 * tables, e.g. on the Tasks or Problem viewer.
 * </p>
 * 
 * @author Leif Frenzel
 */
public interface ICompilerOutputItem {

	/**
	 * <p>
	 * constant for compiler output items global for a resource, which have no
	 * line number.
	 * </p>
	 */
	int LINE_UNSPECIFIED = -1;

	void populateMarker(IMarker marker) throws CoreException;

	int getLine();

	int getStartColumn();

	int getEndColumn();

	String getComment();

}