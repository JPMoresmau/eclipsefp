// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

/** <p>contains static helping functionality to analyze multiple files in
  * a selection for launching interactive sessions.</p>
  *
  * @author Leif Frenzel
  */
class SelectionAnalyzer {

  static IFile[] getSourcesToLoad( final IResource[] resources )
                                                          throws CoreException {
    // pass only source files to the interactive session
    IFile[] result = filterHsFiles( resources );
    if( result.length == 0 && resources.length > 0 ) {
      // check the special cases
      IResource resource = resources[ 0 ];
      IProject project = resource.getProject();
      if(ResourceUtil.hasHaskellNature (project) ) {
        // if source folder or a source subfolder, use exactly the source
        // files in that folder
        if( containsSources( resource ) ) {
          result = getAllSources( ( IFolder )resource );
        } else {
          // use all sources in the project
          result = collectAllSources( project );
        }
      }
    }
    // if nothing in particular could be found, we nonetheless launch
    // the session (with nothing loaded)
    return result;
  }


  // helping methods
  //////////////////

  private static IFile[] collectAllSources( final IProject project )
                                                          throws CoreException {
    //IContainer sourceContainer = ResourceUtil.getSourceFolder( project );
    List<IResource> list = new ArrayList<>();
    for (IContainer sourceContainer:ResourceUtil.getSourceFolders( project )){
      collectAllSourcesRec( list, sourceContainer );
    }
    return toArray( list );
  }

  private static void collectAllSourcesRec( final List<IResource> list,
                                            final IContainer container )
                                                          throws CoreException {
    IResource[] members = container.members();
    for( int i = 0; i < members.length; i++ ) {
      if( members[ i ] instanceof IContainer ) {
        collectAllSourcesRec( list, ( IContainer )members[ i ] );
      } else if( isHsFile( members[ i ] ) ) {
        list.add( members[ i ] );
      }
    }
  }

  private static IFile[] getAllSources( final IFolder folder )
                                                          throws CoreException {
    IResource[] ress = folder.members();
    return filterHsFiles( ress );
  }

  private static IFile[] filterHsFiles( final IResource[] resources ) {
    ArrayList<IResource> list = new ArrayList<>();
    for (int i = 0; i < resources.length; i++) {
      IResource res = resources[ i ];
      if( isHsFile( res ) ) {
        list.add( res );
      }
    }
    return toArray( list );
  }

  private static boolean isHsFile( final IResource res )  {
    return    res != null
           && res instanceof IFile
           && isInHsProject( res )
           && FileUtil.hasHaskellExtension( res );
  }

  private static boolean isInHsProject( final IResource res ){
    IProject project = res.getProject();
    return ResourceUtil.hasHaskellNature (project);
  }

  /** returns true iff the passed resource is either the source folder or
    * a subfolder of the source folder of its project. */
  private static boolean containsSources( final IResource resource ) {
    boolean result = false;
    if( resource instanceof IFolder ) {
      IFolder folder = ( IFolder )resource;
      if( ResourceUtil.isSourceFolder( folder ) ) {
        result = true;
      } else {
        result = containsSources( folder.getParent() );
      }
    }
    return result;
  }

  private static IFile[] toArray( final List<IResource> list ) {
    IFile[] result = new IFile[ list.size() ];
    list.toArray( result );
    return result;
  }
}
