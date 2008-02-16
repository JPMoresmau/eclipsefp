// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;


/** <p>checks if the output folder and binary folder exists and creates them,
  * if necessary.</p>
  *
  * @author Leif Frenzel
  */
public class CheckOutFoldersOperation implements IWorkspaceRunnable {

  private final IProject project;

  CheckOutFoldersOperation( final IProject project ) {
    this.project = project;
  }


  // interface methods of IWorkspaceRunnable
  //////////////////////////////////////////

  public void run( final IProgressMonitor monitor ) throws CoreException {
    monitor.beginTask( CoreTexts.checkOutFoldersOperation_cleaning, 15 );
    try {
      checkOutFolder( monitor ); // (50)
      checkBinFolder( monitor ); // (50)
    } finally {
      monitor.done();
    }
  }


  // helping methods
  //////////////////

  private void checkOutFolder( final IProgressMonitor monitor )
                                                          throws CoreException {
    monitor.subTask( CoreTexts.checkOutFoldersOperation_checkingOutput );
    IContainer outFolder = ResourceUtil.getOutFolder( project );
    create( outFolder, monitor );
  }

  private void checkBinFolder( final IProgressMonitor monitor )
                                                          throws CoreException {
    monitor.subTask( CoreTexts.checkOutFoldersOperation_checkingBin );
    IContainer binFolder = ResourceUtil.getBinFolder( project );
    create( binFolder, monitor );
  }

  private void create( final IContainer container,
                       final IProgressMonitor monitor ) throws CoreException {
    if( mustCreate( container ) ) {
      IPath path = container.getProjectRelativePath();
      IFolder folder = project.getFolder( path );
      folder.create( true, true, new SubProgressMonitor( monitor, 50 ) );
    }
  }

  private boolean mustCreate( final IContainer container ) {
    return    container != null
           && !container.equals( project )
           && !container.exists();
  }
}