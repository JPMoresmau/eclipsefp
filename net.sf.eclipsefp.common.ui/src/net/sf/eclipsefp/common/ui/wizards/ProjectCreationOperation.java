// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.wizards;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.common.ui.CommonUIPlugin;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.operation.IRunnableWithProgress;



/** <p>creates the new project.</p>
  *
  * @author Leif Frenzel 
  */
public class ProjectCreationOperation implements IRunnableWithProgress {

  private ProjectCreationInfo info;
  
  public ProjectCreationOperation( final ProjectCreationInfo info ) {
    this.info = info;
  }
  
  public void run( final IProgressMonitor passedMon ) 
                                             throws InvocationTargetException, 
                                                    InterruptedException {
    IProgressMonitor mon = ( passedMon == null ) ? new NullProgressMonitor()
                                                 : passedMon;
    IWorkspaceRunnable operation = new IWorkspaceRunnable() {
      public void run( final IProgressMonitor monitor ) throws CoreException {
        monitor.beginTask( "Processing ...", 
                           info.getDirectories().length + 3 );
      
        monitor.subTask( "Creating project ..." );
        IProject project = createProjectResource();
        monitor.worked( 1 );

        monitor.subTask( "Adding natures ..." );
        addNatures( monitor, project );         
      
        monitor.subTask( "Creating directory structure ..." );
        createDirectories( monitor, project );
        
        monitor.subTask( "Updating project settings ..." );
        createDescriptionFile( monitor, project );
      }
    };
    try {
      ResourcesPlugin.getWorkspace().run( operation, mon );
    } catch( CoreException cex ) {
      CommonUIPlugin.log( "Problem creating new project.", cex );
    } finally {
      mon.done();
    }
  }

  
  // helping methods
  //////////////////
  
  private IProject createProjectResource() throws CoreException {
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    final String projectName = info.getProjectName();
    final String projectLocation = info.getProjectLocation();
    
    IProject result = root.getProject( projectName );
    IProjectDescription desc = null;
    
    if(isDefaultLocation( projectLocation )) {
      desc = null;
    } else {
      desc = result.getWorkspace().newProjectDescription(projectName);
      desc.setLocation(new Path(projectLocation));
    }
    
    if( !result.exists() ) {
      result.create( desc, null );
    }
    if( !result.isOpen() ) {
      result.open( null );
    }
    return result;
  }

  private boolean isDefaultLocation( final String projectLocation ) {
    return null == projectLocation
        || "".equals(projectLocation)
        || Platform.getLocation().toString().equals(projectLocation);
  }
  
  private void addNatures( final IProgressMonitor mon, 
                           final IProject project ) throws CoreException {
    IProjectDescription desc = project.getDescription();
    // setting null here will apply the default location
    desc.setLocation( null );
    desc.setNatureIds( info.getProjectNatures() );
    project.setDescription( desc, new SubProgressMonitor( mon, 1 ) );
  }

  private void createDirectories( final IProgressMonitor mon, 
                                  final IProject proj ) throws CoreException {
    String[] directories = info.getDirectories();
    for( int i = 0; i < directories.length; i++ ) {
      if( !directories[ i ].equals( "" ) ) {
        IFolder dir = proj.getFolder( directories[ i ] );
        dir.create( true, true, new SubProgressMonitor( mon, 1 ) );
      }
    }
  }
  
  private void createDescriptionFile( final IProgressMonitor monitor, 
                                      final IProject project ) 
                                                          throws CoreException {
    DescriptorFileInfo descFileInfo = info.getDescFileInfo();
    if(    descFileInfo != null
        && !descFileInfo.getName().equals( "" ) ) {
      IFile file = project.getFile( descFileInfo.getName() );
      String content = descFileInfo.getContent();
      InputStream is = new ByteArrayInputStream( content.getBytes() );
      file.create( is, true, new SubProgressMonitor( monitor, 1 ) );
    }
  }
}