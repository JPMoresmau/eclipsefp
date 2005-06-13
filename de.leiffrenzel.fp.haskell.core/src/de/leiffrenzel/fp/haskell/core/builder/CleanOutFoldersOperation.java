// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.builder;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;

import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;


/** <p>Operation for cleaning output and binary folders of a Haskell 
  * project.</p>
  * 
  * @author Leif Frenzel
  */
class CleanOutFoldersOperation implements IWorkspaceRunnable {

  private static IResourceProxyVisitor folderCleaner = new FolderCleaner();
  
  private IProject project;
  
  CleanOutFoldersOperation( final IProject project ) {
    this.project = project;
  }
  
  public void run( final IProgressMonitor mon ) throws CoreException {
    mon.beginTask( "Cleaning output folder", 15 );
    try {
      deleteExe( mon );      //  (3)
      shrubOutFolder( mon ); // (12)
    } finally {
      mon.done();
    }
  }
  
  
  // helping methods
  //////////////////
  
  private void shrubOutFolder( final IProgressMonitor mon ) 
                                                          throws CoreException {
    mon.subTask( "Shrubbing output folder." );
    IContainer outFolder = ResourceUtil.getOutFolder( project );
    if( outFolder != null && !outFolder.equals( project ) ) {
      outFolder.accept( folderCleaner, IContainer.INCLUDE_PHANTOMS );
    }
    mon.worked( 12 );
  }

  private void deleteExe( final IProgressMonitor mon ) throws CoreException {
    mon.subTask( "Removing old executables" );
    IFile file = ResourceUtil.getProjectExecutable( project );
    if( file != null ) {
System.err.println( "Deleting " + file.toString() + "  " + file.getName() );
      file.delete( true, new SubProgressMonitor( mon, 3 ) );
    }
  }
  
  
  // inner classes
  ////////////////
  
  private static class FolderCleaner implements IResourceProxyVisitor {
    public boolean visit( final IResourceProxy proxy ) throws CoreException {
      if( proxy.getType() == IResource.FILE ) {
        IResource resource = proxy.requestResource();
        // TODO need more general approach here
        String name = resource.getName();
        if( !name.equals( ".project" ) && !name.equals( ".hsproject" ) ) {
System.err.println( "Deleting " + resource.toString() );        
          resource.delete( IResource.FORCE, null );
        }
      }
      return true;
    }
  }  
}