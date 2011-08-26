// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.builder.HaskellBuilder;
import net.sf.eclipsefp.haskell.core.hlint.HLintBuilder;
import net.sf.eclipsefp.haskell.core.partitioned.alex.AlexBuilder;
import net.sf.eclipsefp.haskell.core.partitioned.happy.HappyBuilder;
import net.sf.eclipsefp.haskell.core.partitioned.uuagc.UuagcBuilder;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;

/** <p>The project nature that indicates Haskell projects.</p>
 *
 * @author Leif Frenzel
 * @author Alejandro Serrano
 */
public class HaskellNature implements IProjectNature {

  public static final String NATURE_ID = HaskellNature.class.getName();
  private IProject project;


  // interface methods of IProjectNature
  //////////////////////////////////////

  public void configure() throws CoreException {
    addBuilder( HaskellBuilder.BUILDER_ID );
    //addBuilder( CabalBuilder.BUILDER_ID );
    addBuilder( HLintBuilder.BUILDER_ID );
    addBuilder( AlexBuilder.BUILDER_ID );
    addBuilder( HappyBuilder.BUILDER_ID );
    addBuilder( UuagcBuilder.BUILDER_ID );
  }

  public void deconfigure() throws CoreException {
    removeBuilder( UuagcBuilder.BUILDER_ID );
    removeBuilder( HappyBuilder.BUILDER_ID );
    removeBuilder( AlexBuilder.BUILDER_ID );
    removeBuilder( HLintBuilder.BUILDER_ID );
    // removeBuilder( CabalBuilder.BUILDER_ID );
    removeBuilder( HaskellBuilder.BUILDER_ID );
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