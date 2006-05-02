// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.launch;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.project.HaskellNature;
import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;

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
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {
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
    IContainer sourceContainer = ResourceUtil.getSourceFolder( project );
    List list = new ArrayList();
    collectAllSourcesRec( list, sourceContainer );
    return toArray( list );
  }
 
  private static void collectAllSourcesRec( final List list, 
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

  private static IFile[] filterHsFiles( final IResource[] resources ) 
                                                          throws CoreException {
    List list = new ArrayList();
    for (int i = 0; i < resources.length; i++) {
      IResource res = resources[ i ];
      if( isHsFile( res ) ) {
        list.add( res );
      }
    }
    return toArray( list );
  }

  private static boolean isHsFile( final IResource res ) throws CoreException {
    return    res != null
           && res instanceof IFile 
           && isInHsProject( res )
           && ResourceUtil.hasHaskellExtension( res );
  }
  
  private static boolean isInHsProject( final IResource res ) 
                                                          throws CoreException {
    IProject project = res.getProject();
    return project.hasNature( HaskellNature.NATURE_ID );
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
  
  private static IFile[] toArray( final List list ) {
    IFile[] result = new IFile[ list.size() ];
    list.toArray( result );
    return result;
  }
}
