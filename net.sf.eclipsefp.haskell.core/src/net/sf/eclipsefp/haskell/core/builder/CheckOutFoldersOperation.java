// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import org.eclipse.core.resources.*;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;


/** <p>checks if the output folder and binary folder exists and creates them,
  * if necessary.</p>
  * 
  * @author Leif Frenzel
  */
public class CheckOutFoldersOperation implements IWorkspaceRunnable {

  private IProject project;
  
  CheckOutFoldersOperation( final IProject project ) {
    this.project = project;
  }

  
  // interface methods of IWorkspaceRunnable
  //////////////////////////////////////////
  
  public void run( final IProgressMonitor monitor ) throws CoreException {
    monitor.beginTask( "Cleaning output folder", 15 );
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
    monitor.subTask( "Checking output folder" );
    IContainer outFolder = ResourceUtil.getOutFolder( project );
    create( outFolder, monitor );
  }

  private void checkBinFolder( final IProgressMonitor monitor ) 
                                                          throws CoreException {
    monitor.subTask( "Checking binary folder" );
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