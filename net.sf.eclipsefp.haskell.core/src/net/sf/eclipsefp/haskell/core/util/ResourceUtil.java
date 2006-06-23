// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.util;

import java.io.*;

import net.sf.eclipsefp.common.core.util.Assert;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.*;

/**
 * <p>
 * contains static helping functionality to work on file resources in the
 * workspace.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class ResourceUtil {

	/**
	 * <p>
	 * the file extension of Haskell source files.
	 * </p>
	 */
	public static final String EXTENSION_HS = "hs";

	/**
	 * <p>
	 * the file extension of literate Haskell source files.
	 * </p>
	 */
	public static final String EXTENSION_LHS = "lhs";

	/**
	 * <p>
	 * returns whether the passed resource is a Haskell source file, as
	 * recognized by the file extensions '.hs' and '.lhs'.
	 * </p>
	 */
	public static boolean hasHaskellExtension(final IResource resource) {
		return has(resource, EXTENSION_HS) || has(resource, EXTENSION_LHS);
	}

	/**
	 * <p>
	 * returns whether the passed resource is a literate Haskell source file, as
	 * recognized by the file extension '.lhs'.
	 * </p>
	 */
	public static boolean hasLiterateExtension(final IResource resource) {
		return has(resource, EXTENSION_LHS);
	}

	/**
	 * <p>
	 * returns the target executable for the passed project as resource. The
	 * project must have the Haskell nature.
	 * </p>
	 */
	public static IFile getProjectExecutable(final IProject project)
			throws CoreException
	{
		Assert.isTrue(project.hasNature(HaskellNature.NATURE_ID));

		IFile result = null;
		IHaskellProject hsProject = getHsProject(project);

		IPath binPath = hsProject.getBinPath();
		IContainer binContainer = project;
		if (binPath.segmentCount() > 0) {
			binContainer = project.getFolder(binPath);
		}
		IResource[] members = binContainer.members();
		for (int i = 0; i < members.length; i++) {
			String targetName = hsProject.getTargetName();
			if (members[i].getType() == IResource.FILE
					&& members[i].getName().startsWith(targetName)) {
				result = (IFile) members[i];
			}
		}
		return result;
	}

	/**
	 * <p>
	 * returns the output folder of the passed project as resource. The project
	 * must have the Haskell nature.
	 * </p>
	 */
	public static IContainer getOutFolder(final IProject project)
			throws CoreException
	{
		Assert.isTrue(project.hasNature(HaskellNature.NATURE_ID));

		IPath outputPath = getHsProject(project).getOutputPath();
		IContainer result;
		if (outputPath.equals(project.getProjectRelativePath())) {
			result = project;
		} else {
			result = project.getFolder(outputPath);
		}
		return result;
	}

	/**
	 * <p>
	 * returns the binary folder of the passed project as resource. The project
	 * must have the Haskell nature.
	 * </p>
	 */
	public static IContainer getBinFolder(final IProject project)
			throws CoreException
	{
		Assert.isTrue(project.hasNature(HaskellNature.NATURE_ID));

		IPath binPath = getHsProject(project).getBinPath();
		IContainer result;
		if (binPath.equals(project.getProjectRelativePath())) {
			result = project;
		} else {
			result = project.getFolder(binPath);
		}
		return result;
	}

	/**
	 * <p>
	 * returns the source folder of the passed project as resource. The project
	 * must have the Haskell nature.
	 * </p>
	 */
	public static IContainer getSourceFolder(final IProject project)
			throws CoreException
	{
		Assert.isTrue(project.hasNature(HaskellNature.NATURE_ID));
		IContainer result;
		IPath sourcePath = getHsProject(project).getSourcePath();
		if (sourcePath.equals(project.getProjectRelativePath())) {
			result = project;
		} else {
			result = project.getFolder(sourcePath);
		}
		return result;
	}

	/**
	 * <p>
	 * reads an input stream and returns the contents as String.
	 * </p>
	 */
	public static String readStream(final InputStream is) throws IOException {
		StringBuffer sbResult = new StringBuffer();
		BufferedReader br = new BufferedReader(new InputStreamReader(is));
		String line = br.readLine();
		while (line != null) {
			sbResult.append(line);
			// Note: this could in some cases obscure the positions of elements
			// in
			// the code. It is no problem as long as all source positions we get
			// from the parser are in terms of line/column, but it would make a
			// difference if we got them in terms of offset/length
			sbResult.append("\n");
			line = br.readLine();
		}
		br.close();
		is.close();

		return sbResult.toString();
	}

	/**
	 * finds the corresponding resource for the specified element. This is
	 * element itself, if it is an IResource, or an adapter. Returns null, if no
	 * resource could be found.
	 */
	public static IResource findResource(final Object element) {
		IResource result = null;
		if (element instanceof IResource) {
			result = (IResource) element;
		} else if (element instanceof IAdaptable) {
			Object adapter = ((IAdaptable) element).getAdapter(IResource.class);
			if (adapter instanceof IResource) {
				result = (IResource) adapter;
			}
		}
		return result;
	}

	/**
	 * returns whether the specified file is the project executable of its
	 * project.
	 */
	public static boolean isProjectExecutable(final IFile file) {
		boolean result = false;
		try {
			IFile exe = ResourceUtil.getProjectExecutable(file.getProject());
			result = file.equals(exe);
		} catch (CoreException ex) {
			String msg = "Problem determining project executable for "
					+ file.getName();
			HaskellCorePlugin.log(msg, ex);
		}
		return result;
	}

	/**
	 * <p>
	 * returns whether the specified folder is the source folder of its
	 * (Haskell) project.
	 * </p>
	 */
	public static boolean isSourceFolder(final IFolder folder) {
		IProject project = folder.getProject();
		IHaskellProject hsProject = HaskellProjectManager.get(project);
		IPath sourcePath = hsProject.getSourcePath();
		IPath folderPath = folder.getProjectRelativePath();
		return sourcePath.equals(folderPath);
	}

	public static boolean isInHaskellProject(final IResource resource) {
		boolean result = false;
		IProject project = resource.getProject();
		try {
			result = project.hasNature(HaskellNature.NATURE_ID);
		} catch (CoreException cex) {
			// ignore, we must assume this is no Haskell project then
		}
		return result;
	}

	// helping methods
	// ////////////////

	private static boolean has(final IResource resource,
		final String extension)
	{
		String resExt = resource.getFileExtension();
		return resExt != null && resExt.equalsIgnoreCase(extension);
	}

	private static IHaskellProject getHsProject(final IProject project) {
		return HaskellProjectManager.get(project);
	}

	public static String getModuleName(String fileName) {
		return fileName.substring(0, fileName.lastIndexOf('.'));
	}

}