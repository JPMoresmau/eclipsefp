// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;

/**
 * <p>
 * A Haskell source file.
 * </p>
 * 
 * @author Leif Frenzel
 */
public interface ICompilationUnit extends IAdaptable {

	/**
	 * <p>
	 * returns the modules contained in this compilation unit.
	 * </p>
	 */
	IModule[] getModules();

	/**
	 * <p>
	 * returns the <code>IFile</code> that is represented by this compilation
	 * unit.
	 * </p>
	 */
	IFile getUnderlyingResource();

	// source location handling
	// /////////////////////////

	/**
	 * <p>
	 * returns the source location of the next element (not the element at the
	 * location, but that element's successor) after the specified location, or
	 * <code>null</code>, if there is no more element after the location, of
	 * if the passed location is invalid.
	 * </p>
	 */
	ISourceLocation getNextLocation(ISourceLocation srcLoc);

	String getOriginalSourceCode();
}
