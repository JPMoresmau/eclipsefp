// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;


/** <p>Operation for cleaning output and binary folders of a Haskell
  * project.</p>
  *
  * @author Leif Frenzel
  */
class CleanOutFoldersOperation implements IWorkspaceRunnable {

  private static IResourceProxyVisitor folderCleaner = new FolderCleaner();

  private final IProject project;

  CleanOutFoldersOperation( final IProject project ) {
    this.project = project;
  }

  public void run( final IProgressMonitor mon ) throws CoreException {
    mon.beginTask( CoreTexts.cleanOutFoldersOperation_cleaning, 15 );
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
    mon.subTask( CoreTexts.cleanOutFoldersOperation_shrubbingOut );
    IContainer outFolder = ResourceUtil.getOutFolder( project );
    if( outFolder != null && !outFolder.equals( project ) ) {
      outFolder.accept( folderCleaner, IContainer.INCLUDE_PHANTOMS );
    }
    mon.worked( 12 );
  }

  private void deleteExe( final IProgressMonitor mon ) throws CoreException {
    mon.subTask( CoreTexts.cleanOutFoldersOperation_removingExes );
    IFile[] files = ResourceUtil.getProjectExecutables( project );
    for( IFile file: files ) {
      if( file != null && file.isAccessible() ) {
        file.delete( true, new SubProgressMonitor( mon, 3 ) );
      }
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
        if( !name.equals( ".project" ) && !name.equals( ".hsproject" ) ) { //$NON-NLS-1$ //$NON-NLS-2$
          resource.delete( IResource.FORCE, null );
        }
      }
      return true;
    }
  }
}