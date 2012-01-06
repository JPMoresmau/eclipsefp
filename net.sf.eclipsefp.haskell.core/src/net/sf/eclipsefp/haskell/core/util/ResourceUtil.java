// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component;
import net.sf.eclipsefp.haskell.buildwrapper.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.ModuleInclusionType;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;

/**
 * <p>
 * contains static helping functionality to work on file resources in the
 * workspace.
 * </p>
 *
 * @author Leif Frenzel
 */
public class ResourceUtil {

  public static boolean hasHaskellNature(final IProject p){
    try {
      return p.hasNature( HaskellNature.NATURE_ID );
    } catch (CoreException ce){
      HaskellCorePlugin.log( ce );
    }
    return false;
  }

  static Map<String,IFile> getExecutablesOfComponentType(final IProject project, final ComponentType type) throws CoreException {
    Map<String,IFile> result = new HashMap<String, IFile>();
    if (project.hasNature( HaskellNature.NATURE_ID ) ){
      /*ScionInstance instance=ScionPlugin.getScionInstance( project );
      if (instance!=null){
        for (Component c:instance.getComponents()){
          if (c.isBuildable() &&  c.getType().equals( type )){
            String name=FileUtil.makeExecutableName( c.getName() );
            IFile f=project.getFile( ScionPlugin.DIST_FOLDER+File.separator+"build"+File.separator+c.getName()+File.separator+name ); //$NON-NLS-1$
            if (f.exists()){
              result.add( f );
            }
          }
        }
      }*/
      BWFacade facade=BuildWrapperPlugin.getFacade( project );
      if (facade!=null){
        for (Component c:facade.getComponents()){
          if (c.isBuildable() &&  c.getType().equals( type )){
            IFile f = getExecutableLocation( project, c.getName() );
            if (f.exists()){
              result.put(c.getName(), f );
            }
          }
        }
      }
    }

    return result;
  }

  public static IFile getExecutableLocation (final IProject project, final String componentName) {
    String name=FileUtil.makeExecutableName( componentName );
    String exe = BWFacade.DIST_FOLDER + File.separator + "dist" + File.separator +  //$NON-NLS-1$
        "build" + File.separator + componentName + File.separator + name; //$NON-NLS-1$
    return project.getFile( exe );
  }

  /**
   * Return the project executable as an ArrayList, assuming that the
   * project has the Haskell nature.
   */

	public static Map<String,IFile> getProjectExecutables( final IProject project)
	  throws CoreException
	{
	  return getExecutablesOfComponentType( project, ComponentType.EXECUTABLE );
	}

	public static Map<String,IFile> getProjectTestSuites( final IProject project)
	  throws CoreException
  {
    return getExecutablesOfComponentType( project, ComponentType.TESTSUITE );
  }



//	public static boolean isProjectExecutable(final IProject project, final String exeName)
//	{
//	  boolean retval = false;
//	  String theExeName = FileUtil.makeExecutableName(exeName);
//
//	  try {
//	    ArrayList<IFile> executables = getProjectExecutablesArray(project);
//	    for (IFile iter: executables) {
//	      if (iter.getName().equals( theExeName )) {
//	        retval = true;
//	      }
//	    }
//	  } catch (CoreException e) {
//	    retval = false;
//	  }
//
//	  return retval;
//	}

	/**
	 * <p>
	 * returns the target executable for the passed project as resource. The
	 * project must have the Haskell nature.
	 * </p>
	 */
	public static IFile[] getProjectExecutablesArray( final IProject project )
      throws CoreException {
	  Map<String,IFile> executables = getProjectExecutables(project);
    return executables.values().toArray( new IFile[ executables.size() ] );
  }

	public static IFile[] getProjectTestSuitesArray( final IProject project )
      throws CoreException {
	  Map<String,IFile> executables = getProjectTestSuites( project );
    return executables.values().toArray( new IFile[ executables.size() ] );
  }

	/**
	 * Get the output folder of the Haskell project.
	 *
	 * @param project The Eclipse project object
	 * @return The IContainer object corresponding to the project's output
	 * folder.
	 */
//	public static IContainer getOutFolder(final IProject project)
//			throws CoreException
//	{
//	  Assert.isNotNull( project );
//		Assert.isTrue(project.hasNature(HaskellNature.NATURE_ID));
//
//		IHaskellProject hsProject = getHsProject(project);
//		IPath outputPath = hsProject.getOutputPath();
//		IContainer result;
//		if (outputPath.equals(project.getProjectRelativePath())) {
//			result = project;
//		} else {
//			result = project.getFolder(outputPath);
//		}
//		return result;
//	}

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

        IFile f=BuildWrapperPlugin.getCabalFile( project );
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

  public static IResource[] getResourcesFromSelection(final ISelection selection){
    if( selection != null && selection instanceof IStructuredSelection ) {
      List<IResource> list = new ArrayList<IResource>();
      IStructuredSelection ssel = ( IStructuredSelection )selection;
      for( Object element: ssel.toList() ) {
        IResource res = ResourceUtil.findResource( element );
        if( res != null ) {
         list.add( res );
        }
      }
      IResource[] ress = toResourceArray( list );
      return ress;
    }
    return new IResource[0];
  }


  public static IResource[] toResourceArray( final List<IResource> list ) {
    IResource[] result = new IResource[ list.size() ];
    list.toArray( result );
    return result;
  }

	private static IContainer getContainer(final IProject p,final String src){
	  return src.equals( "." )?p:p.getFolder( src ); //$NON-NLS-1$
	}

	/**
	 * Predicate that tests if the specified folder is one of the Haskell
	 * project's source folders.
	 *
	 * @return True, if the folder is a source folder in the Haskell project.
	 */
	public static boolean isSourceFolder( final IFolder folder ) {
    IProject project = folder.getProject();
    /*IHaskellProject hsProject = HaskellProjectManager.get( project );
    IPath folderPath = folder.getProjectRelativePath();
    return hsProject.getSourcePaths().contains( folderPath );
    */
    try {
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {
        IFile f=BuildWrapperPlugin.getCabalFile( project );
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

	public static boolean isInSourceFolder( final IFile file ) {
	  if( file == null || !file.isAccessible() ) {
      return false;
    }
	  Collection<IContainer> sourcePaths =ResourceUtil.getSourceFolders( file.getProject() );
    for( IContainer sourcePath: sourcePaths ) {
      if (sourcePath.getLocation().isPrefixOf( file.getLocation())){
        return true;
      }
    }
    return false;
	}

	public static boolean isInHaskellProject(final IResource resource) {
		boolean result = false;
		if (resource != null) {
  		IProject project = resource.getProject();
  		try {
  			result = project.hasNature(HaskellNature.NATURE_ID);
  		} catch (CoreException cex) {
  			// ignore, we must assume this is not a Haskell project
  		}
		}
		return result;
	}

  public static IContainer getSourceContainer( final IResource resource ) {
    IProject project = resource.getProject();
    try {
      if(project.exists() && project.hasNature( HaskellNature.NATURE_ID ) ) {

        IFile f=BuildWrapperPlugin.getCabalFile( project );
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

  public static Collection<String> getHiddenImportPackages(final IFile[] files){
    Collection<String> ips=getImportPackages(files);
    Collection<String> hidden=new HashSet<String>();
    if (ips.size()>0){
      Map<String,CabalPackage[]> pkgs=BuildWrapperPlugin.getFacade( files[0].getProject() ).getPackagesByDB();
        //ScionPlugin.getScionInstance( files[0] ).getPackagesByDB();
      if (pkgs!=null){
        for (CabalPackage[] cps:pkgs.values()){
          for (CabalPackage cp:cps){
            if (cp.getComponents().length>0 && ips.contains(cp.getName()) && !cp.isExposed()){
              hidden.add(cp.getName());
            }
          }
        }
      }
    }
    return hidden;

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
    // if we put them in a set, it messes up options that are made of two words, like -package ghc
    // hopefully duplication of option will not be an issue
    Collection<String> ret=new ArrayList<String>();

    Set<PackageDescriptionStanza> applicable=getApplicableStanzas( files );

    for (PackageDescriptionStanza pds:applicable){
      ret.addAll( PackageDescriptionLoader.parseList( pds.getProperties().get( field ) ) );
    }

    return ret;
  }

  public static IFile findFileFromModule(final IProject project,final String module){
    try {
        String path=module.replace( '.', '/' );
        if( project.hasNature( HaskellNature.NATURE_ID ) ) {
          IFile f=BuildWrapperPlugin.getCabalFile( project );
          PackageDescription pd=PackageDescriptionLoader.load(f);
          Map<String,List<PackageDescriptionStanza>> stzs=pd.getStanzasBySourceDir();

            for (String src:stzs.keySet()){
              IContainer fldr=getContainer(project,src);
              for (String ext:FileUtil.haskellExtensions){
                IFile file=fldr.getFile( new Path( path+"."+ext ) ); //$NON-NLS-1$
                if (file.exists()){
                  return file;
                }
              }
            }
        }
      } catch( CoreException ex ) {
        HaskellCorePlugin.log( "getModuleName:", ex ); //$NON-NLS-1$
      }
      return null;

  }

  public static String getModuleName(final IFile file){
    IProject project = file.getProject();
    try {
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {

        IFile f=BuildWrapperPlugin.getCabalFile( project );
        PackageDescription pd=PackageDescriptionLoader.load(f);
        Map<String,List<PackageDescriptionStanza>> stzs=pd.getStanzasBySourceDir();
        if (FileUtil.hasHaskellExtension(file)){
          for (String src:stzs.keySet()){
            IContainer fldr=getContainer(project,src);
            if (file.getProjectRelativePath().toOSString().startsWith( fldr.getProjectRelativePath().toOSString() )){
                for (PackageDescriptionStanza stz:stzs.get(src)){
                  String module=getQualifiedModuleName( file, fldr );
                  if (!ModuleInclusionType.MISSING.equals( stz.getModuleInclusionType( module ) )){
                   return module;
                  }
                }
              }
            }
          }
      }

    } catch( CoreException ex ) {
      HaskellCorePlugin.log( "getModuleName:", ex ); //$NON-NLS-1$
    }
    return ""; //$NON-NLS-1$
  }

  public static Set<PackageDescriptionStanza> getApplicableStanzas(final IFile[] files){
    if (files==null || files.length==0){
      return Collections.emptySet();
    }
    IProject project = files[0].getProject();
    try {
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {

        IFile f=BuildWrapperPlugin.getCabalFile( project );
        PackageDescription pd=PackageDescriptionLoader.load(f);
        Map<String,List<PackageDescriptionStanza>> stzs=pd.getStanzasBySourceDir();

        Set<PackageDescriptionStanza> applicable=new HashSet<PackageDescriptionStanza>();

        for (IFile fi:files){
          for (String src:stzs.keySet()){
            IContainer fldr=getContainer(project,src);
            if (fi.getProjectRelativePath().toOSString().startsWith( fldr.getProjectRelativePath().toOSString() )){

              if (FileUtil.hasHaskellExtension(fi)){
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
      HaskellCorePlugin.log( "getApplicableStanzas:", ex ); //$NON-NLS-1$
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
      return file.getFullPath();
//      String msg =   file.getFullPath()
//                   + " is in no source folder in project " //$NON-NLS-1$
//                   + file.getProject().getName();
//      throw new IllegalArgumentException( msg );
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

//	private static IHaskellProject getHsProject( final IProject project ) {
//    return HaskellProjectManager.get( project );
//  }

  public static String getModuleName( final String fileName ) {
    return fileName.substring( 0, fileName.lastIndexOf( '.' ) );
  }

  public static String getQualifiedModuleName (final IFile file,final IContainer source){
    IPath path=file.getProjectRelativePath().removeFirstSegments( source.getProjectRelativePath().segmentCount() );
    String s=path.toString();
    return getModuleName(s).replace( '/', '.' );
  }

  public static Collection<IProject> getProjects(final ISelection arg1 ){
    Set<IProject> projects=new LinkedHashSet<IProject>();
    if (arg1 instanceof IStructuredSelection){
      for (Iterator<?> it=((IStructuredSelection)arg1).iterator();it.hasNext();){
        IResource res = ResourceUtil.findResource( it.next() );
        try {
          if( res != null && res.getProject()!=null  && res.getProject().hasNature(HaskellNature.NATURE_ID)) {
            projects.add( res.getProject() );
          }
        } catch (CoreException cex) {
          // ignore, we must assume this is not a Haskell project
        }
      }
    }
    return projects;
  }

  public static IProject[] getHaskellProjects( final IWorkspaceRoot root ) {
    List<IProject> list = new ArrayList<IProject>();
    for( IProject project:root.getProjects() ) {
       if(  project.isOpen()
             && hasHaskellNature( project )) {
          list.add( project );
        }
    }
    IProject[] result = new IProject[ list.size() ];
    list.toArray( result );
    return result;
  }
}