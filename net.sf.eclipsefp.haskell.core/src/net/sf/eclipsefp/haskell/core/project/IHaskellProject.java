// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;

/**
 * <p>
 * An IHaskellProject encapsulates Haskell project specific information about a
 * project in the workspace. The project is expected to have the
 * {@link HaskellNature Haskell project nature}.
 * </p>
 *
 * @author Leif Frenzel
 */
public interface IHaskellProject extends IAdaptable {

	String PROPERTY_SOURCE_PATH = "sourcePath"; //$NON-NLS-1$
	String PROPERTY_OUTPUT_PATH = "outputPath"; //$NON-NLS-1$
	String PROPERTY_BIN_PATH = "binPath"; //$NON-NLS-1$
	String PROPERTY_TARGET_NAME = "targetName"; //$NON-NLS-1$
	String PROPERTY_IMPORT_LIBRARIES = "importLibraries"; //$NON-NLS-1$

	/**
	 * <p>
	 * returns the underlying project resource of this IHaskellProject.
	 * </p>
	 */
	IProject getResource();

	/**
	 * <p>
	 * returns the path to the source folder of this IHaskellProject.
	 * </p>
	 */
	IPath getSourcePath();

	/**
	 * <p>
	 * returns the path to the output folder of this IHaskellProject.
	 * </p>
	 */
	IPath getOutputPath();

	/**
	 * <p>
	 * returns the path to the binaries folder of this IHaskellProject (the path
	 * where the built executables are).
	 * </p>
	 */
	IPath getBinPath();

	/**
	 * <p>
	 * returns the name of the target binary for this Haskell project. The
	 * target name may be empty, but must not be null.
	 * </p>
	 */
	String getTargetName();

	/**
	 * <p>
	 * returns the import libraries for this Haskell project.
	 * </p>
	 */
	IImportLibrary[] getImportLibraries();

	IContainer getSourceFolder();

	IHaskellCompiler getCompiler();

	ICompilerOutput compile(IFile file);
}
