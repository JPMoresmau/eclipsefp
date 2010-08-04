// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.ModuleInclusionType;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.internal.util.Assert;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;

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

	public static final String NL = System.getProperty( "line.separator","\n"); //$NON-NLS-1$ //$NON-NLS-2$

	/**
	 * <p>
	 * returns whether the passed resource is a Haskell source file, as
	 * recognized by the file extensions '.hs' and '.lhs'.
	 * </p>
	 */
	public static boolean hasHaskellExtension( final IResource resource ) {
    return has( resource, EXTENSION_HS ) || has( resource, EXTENSION_LHS );
  }

	 public static boolean hasCabalExtension( final IResource resource ) {
	    return has( resource, EXTENSION_CABAL );
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
      if (!path.isEmpty()){
        IFile file = project.getFile( path );
        if( file.exists() ) {
          result.add( file );
        }
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
	 * returns the source folder of the passed project as resource. The project
	 * must have the Haskell nature.
	 * </p>
	 */
	/*public static IContainer getSourceFolder(final IProject project)
	{
		return getHsProject(project).getSourceFolder();
	}*/

	public static Collection<IContainer> getSourceFolders(final IProject project){
	  try {
  	  if( project.hasNature( HaskellNature.NATURE_ID ) ) {

        IFile f=ScionInstance.getCabalFile( project );
        PackageDescription pd=PackageDescriptionLoader.load(f);
        Map<String,List<PackageDescriptionStanza>> stzs=pd.getStanzasBySourceDir();
        Collection<IContainer> ret=new ArrayList<IContainer>();
        for (String s:stzs.keySet()){
          ret.add(getContainer( project,s ));
        }
        return ret;
  	  }
    } catch( CoreException ex ) {
      HaskellCorePlugin.log( "getSourceFolders:"+project, ex ); //$NON-NLS-1$
    }
    return Collections.emptyList();
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
	 * @deprecated
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

	private static IContainer getContainer(final IProject p,final String src){
	  return src.equals( "." )?p:p.getFolder( src ); //$NON-NLS-1$
	}

	/**
	 * <p>
	 * returns whether the specified folder is one of the source folders of its
	 * (Haskell) project.
	 * </p>
	 */
	public static boolean isSourceFolder( final IFolder folder ) {
    IProject project = folder.getProject();
    /*IHaskellProject hsProject = HaskellProjectManager.get( project );
    IPath folderPath = folder.getProjectRelativePath();
    return hsProject.getSourcePaths().contains( folderPath );
    */
    try {
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {
        IFile f=ScionInstance.getCabalFile( project );
        PackageDescription pd=PackageDescriptionLoader.load(f);
        for (String src:pd.getStanzasBySourceDir().keySet()){

         if (getContainer(project,src).equals(folder)){
            return true;
          }
        }
      }

    } catch( CoreException ex ) {
      HaskellCorePlugin.log( "isSourceFolder:", ex ); //$NON-NLS-1$
    }
    return false;
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

  public static IContainer getSourceContainer( final IResource resource ) {
    IProject project = resource.getProject();
    try {
      if(project.exists() && project.hasNature( HaskellNature.NATURE_ID ) ) {

        IFile f=ScionInstance.getCabalFile( project );
        PackageDescription pd=PackageDescriptionLoader.load(f);
        for (String src:pd.getStanzasBySourceDir().keySet()){
          if (src!=null && src.equals( "." )) { //$NON-NLS-1$
            return project;
          }
          IFolder fldr=project.getFolder( src );

          if (resource.getProjectRelativePath().toOSString().startsWith( fldr.getProjectRelativePath().toOSString() )){
            return fldr;
          }
        }
      }

    } catch( CoreException ex ) {
      HaskellCorePlugin.log( "getSourceContainer:"+resource, ex ); //$NON-NLS-1$
    }
    return null;
  }

  public static Collection<String> getImportPackages(final IFile[] files){
    if (files==null || files.length==0){
      return Collections.emptySet();
    }
    Collection<String> ret=new HashSet<String>();

    Set<PackageDescriptionStanza> applicable=getApplicableStanzas( files );

    for (PackageDescriptionStanza pds:applicable){
      ret.addAll(pds.getDependentPackages());
    }

    return ret;
  }

  public static Collection<String> getSourceFolders(final IFile[] files){
    if (files==null || files.length==0){
      return Collections.emptySet();
    }
    Collection<String> ret=new HashSet<String>();

    Set<PackageDescriptionStanza> applicable=getApplicableStanzas( files );

    for (PackageDescriptionStanza pds:applicable){
      ret.addAll(pds.getSourceDirs());
    }

    return ret;
  }

  public static Collection<String> getApplicableListProperty(final IFile[] files,final CabalSyntax field){
    if (files==null || files.length==0){
      return Collections.emptySet();
    }
    Collection<String> ret=new HashSet<String>();

    Set<PackageDescriptionStanza> applicable=getApplicableStanzas( files );

    for (PackageDescriptionStanza pds:applicable){
      ret.addAll( PackageDescriptionLoader.parseList( pds.getProperties().get( field ) ) );
    }

    return ret;
  }

  public static Set<PackageDescriptionStanza> getApplicableStanzas(final IFile[] files){
    if (files==null || files.length==0){
      return Collections.emptySet();
    }
    IProject project = files[0].getProject();
    try {
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {

        IFile f=ScionInstance.getCabalFile( project );
        PackageDescription pd=PackageDescriptionLoader.load(f);
        Map<String,List<PackageDescriptionStanza>> stzs=pd.getStanzasBySourceDir();

        Set<PackageDescriptionStanza> applicable=new HashSet<PackageDescriptionStanza>();

        for (IFile fi:files){
          for (String src:stzs.keySet()){
            IContainer fldr=getContainer(project,src);
            if (fi.getProjectRelativePath().toOSString().startsWith( fldr.getProjectRelativePath().toOSString() )){

              if (hasHaskellExtension(fi)){
                for (PackageDescriptionStanza stz:stzs.get(src)){
                  String module=getQualifiedModuleName( fi, fldr );
                  if (!ModuleInclusionType.MISSING.equals( stz.getModuleInclusionType( module ) )){
                    applicable.add(stz);
                  }
                }
              } else {
                applicable.addAll(stzs.get(src));
              }
            }
          }
        }
        return applicable;
      }

    } catch( CoreException ex ) {
      HaskellCorePlugin.log( "getImportLibraries:", ex ); //$NON-NLS-1$
    }
    return Collections.emptySet();
  }


  public static IPath getSourceRelativePath( final IResource resource ) {
    IPath result = null;
    IContainer sourceFolder = getSourceContainer( resource );
    if( sourceFolder != null ) {
      if( resource != null ) {
        result = getSourceRelativePath( sourceFolder, resource );
      }
    }
    return result;
  }

  public static IPath getSourceRelativePath( final IContainer sourceContainer,
      final IResource resource ) {
      IPath result = null;
      IContainer resourceContainer = getContainer( resource );
      IPath sourcePath = sourceContainer.getProjectRelativePath();
      IPath resourcePath = resourceContainer.getProjectRelativePath();
      if( sourcePath.isPrefixOf( resourcePath ) ) {
        int count = sourcePath.segmentCount();
        result = resourcePath.removeFirstSegments( count );
      }
      return result;
    }

  /** returns the container this resource is in (the resource itself, if it is
   * a container). */
 private static IContainer getContainer( final IResource resource ) {
   return ( resource instanceof IContainer ) ? ( IContainer )resource
                                             : resource.getParent();
 }

	/** <p>returns the path of the specified workspace file relative to the
	  * source folder in the Haskell project.</p>
	  *
	  * @param file  a workspace file, must not be <code>null</code>
	  * @param hp    a haskell project, must not be <code>null</code>
	  */
	public static IPath getSourceFolderRelativeName( final IResource file ) {
    if( file == null  ) {
      throw new IllegalArgumentException();
    }
    IPath projectRelPath = file.getProjectRelativePath();
    IContainer sourceContainer = getSourceContainer( file );
    IPath result = null;
    if( sourceContainer != null ) {
      IPath sourcePath = sourceContainer.getProjectRelativePath();
      if( sourcePath.isPrefixOf( projectRelPath ) ) {
        int count = sourcePath.segmentCount();
        result = projectRelPath.removeFirstSegments( count );
      }
    }

    if( result == null ) {
      String msg =   file.getFullPath()
                   + " is in no source folder in project " //$NON-NLS-1$
                   + file.getProject().getName();
      throw new IllegalArgumentException( msg );
    }
    return result;
  }

  public static IFolder mkdirs( final IPath folderPath,
                                final IProject project ) throws CoreException {
    IFolder result = project.getFolder( folderPath );
    List<IFolder> parents = new ArrayList<IFolder>();
    IContainer container = result;
    while( container instanceof IFolder && !container.exists() ) {
      parents.add( ( IFolder )container );
      container = container.getParent();
    }
    Collections.reverse( parents );
    for( IFolder folder: parents ) {
      folder.create( true, true, new NullProgressMonitor() );
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

  public static String getQualifiedModuleName (final IFile file,final IContainer source){
    IPath path=file.getProjectRelativePath().removeFirstSegments( source.getProjectRelativePath().segmentCount() );
    String s=path.toString();
    return getModuleName(s).replace( '/', '.' );
  }
}