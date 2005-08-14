// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.core.project;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;

/** <p>The superclass for FP project natures.</p>
  * 
  * @author Leif Frenzel
  */
public abstract class FPNature implements IProjectNature {

  private IProject project; 

  /** <p>returns the Builder id for the project builder that is configured 
    * with this FP nature.</p> */
  protected abstract String getBuilderID();


  // interface methods of IProjectNature
  //////////////////////////////////////

  public void configure() throws CoreException {
    addBuilder( getBuilderID() );
  }

  public void deconfigure() throws CoreException {
    removeBuilder( getBuilderID() );
  }

  public IProject getProject() {
    return project;
  }

  public void setProject( final IProject project ) {
    this.project = project;
  }
  
  
  // helping methods
  //////////////////
  
  private void addBuilder( final String builderID ) throws CoreException {
    IProjectDescription desc = getProject().getDescription();
    ICommand[] commands = desc.getBuildSpec();
    boolean found = false;
      
    for( int i = 0; !found && i < commands.length; ++i ) {
      String builderName = commands[ i ].getBuilderName(); 
      if( builderName.equals( builderID ) ) {
        found = true;
      }
    }
    if( !found ) { 
      //add builder to project
      ICommand command = desc.newCommand();
      command.setBuilderName( builderID );
      ICommand[] newCommands = new ICommand[ commands.length + 1 ];
      // Add it before other builders.
      System.arraycopy( commands, 0, newCommands, 1, commands.length );
      newCommands[ 0 ] = command;
      desc.setBuildSpec( newCommands );
    }
    getProject().setDescription( desc, null );
  }

  private void removeBuilder( final String builderID ) throws CoreException {
    IProjectDescription description = getProject().getDescription();
    ICommand[] commands = description.getBuildSpec();
    boolean finished = false;
    for( int i = 0; !finished && i < commands.length; ++i ) {
      if( commands[ i ].getBuilderName().equals( builderID ) ) {
        ICommand[] newCommands = new ICommand[ commands.length - 1 ];
        System.arraycopy( commands, 0, newCommands, 0, i );
        System.arraycopy( commands, 
                          i + 1, 
                          newCommands, 
                          i, 
                          commands.length - i - 1 );
        description.setBuildSpec( newCommands );
        getProject().setDescription( description, null );
        finished = true;
      }
    }
  }
}