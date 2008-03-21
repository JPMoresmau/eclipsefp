// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.Assert;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;

/**
 * <p>
 * contains static helping functionality to work on file resources in the
 * workspace.
 * </p>
 *
 * @author Leif Frenzel
 */
public class ResourceUtil {

	public static final String EXTENSION_HS = "hs"; //$NON-NLS-1$
	public static final String EXTENSION_LHS = "lhs"; //$NON-NLS-1$
	public static final String EXTENSION_CABAL = "cabal"; //$NON-NLS-1$

	/**
	 * <p>
	 * returns whether the passed resource is a Haskell source file, as
	 * recognized by the file extensions '.hs' and '.lhs'.
	 * </p>
	 */
	public static boolean hasHaskellExtension( final IResource resource ) {
    return has( resource, EXTENSION_HS ) || has( resource, EXTENSION_LHS );
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
	public static IFile[] getProjectExecutables( final IProject project )
      throws CoreException {
    Assert.isTrue( project.hasNature( HaskellNature.NATURE_ID ) );

    List<IFile> result = new ArrayList<IFile>();
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    Set<IPath> targetNames = hsProject.getTargetNames();
    for( IPath path: targetNames ) {
      IFile file = project.getFile( path );
      if( file.exists() ) {
        result.add( file );
      }
    }
    return result.toArray( new IFile[ result.size() ] );
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
	{
		return getHsProject(project).getSourceFolder();
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
			sbResult.append("\n"); //$NON-NLS-1$
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
	public static boolean isProjectExecutable( final IFile file ) {
	  if( file == null ) {
	    throw new IllegalArgumentException();
	  }
    boolean result = false;
    try {
      IFile[] exes = getProjectExecutables( file.getProject() );
      for( IFile exe: exes ) {
        result |= file.equals( exe );
      }
    } catch( CoreException ex ) {
      String msg = "Problem determining project executable for "; //$NON-NLS-1$
      HaskellCorePlugin.log( msg + file.getName(), ex );
    }
    return result;
  }

	/**
	 * <p>
	 * returns whether the specified folder is one of the source folders of its
	 * (Haskell) project.
	 * </p>
	 */
	public static boolean isSourceFolder( final IFolder folder ) {
    IProject project = folder.getProject();
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    IPath folderPath = folder.getProjectRelativePath();
    return hsProject.getSourcePaths().contains( folderPath );
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

	/** <p>returns the path of the specified workspace file relative to the
	  * source folder in the Haskell project.</p>
	  *
	  * @param file  a workspace file, must not be <code>null</code>
	  * @param hp    a haskell project, must not be <code>null</code>
	  */
	public static String getSourceDirRelativeName( final IFile file,
                                                 final IHaskellProject hp ) {
    if( file == null || hp == null ) {
      throw new IllegalArgumentException();
    }
    IPath projectRelPath = file.getProjectRelativePath();
    String result = null;
	  Iterator<IPath> it = hp.getSourcePaths().iterator();
	  while( result == null && it.hasNext() ) {
	    IPath sourcePath = it.next();
	    if( sourcePath.isPrefixOf( projectRelPath ) ) {
	      int num = projectRelPath.matchingFirstSegments( sourcePath );
	      result = projectRelPath.removeFirstSegments( num ).toOSString();
	    }
	  }
    if( result == null ) {
      String msg =   file.getFullPath()
                   + " is in no source folder in project " //$NON-NLS-1$
                   + hp.getResource().getName();
      throw new IllegalArgumentException( msg );
    }
    return result;
  }


	// helping methods
	// ////////////////

	private static boolean has( final IResource resource, final String extension ) {
    String resExt = resource.getFileExtension();
    return resExt != null && resExt.equalsIgnoreCase( extension );
  }

  private static IHaskellProject getHsProject( final IProject project ) {
    return HaskellProjectManager.get( project );
  }

  public static String getModuleName( final String fileName ) {
    return fileName.substring( 0, fileName.lastIndexOf( '.' ) );
  }
}