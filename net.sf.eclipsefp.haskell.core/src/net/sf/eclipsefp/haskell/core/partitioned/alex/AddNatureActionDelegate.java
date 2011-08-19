package net.sf.eclipsefp.haskell.core.partitioned.alex;

import java.util.Collection;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;


public class AddNatureActionDelegate implements IObjectActionDelegate {

  // private Shell currentShell;
  private IProject project;

  public AddNatureActionDelegate() {
    // Do nothing
  }

  @Override
  public void run( final IAction action ) {
    if( project != null ) {
      try {
        IProjectDescription desc = project.getDescription();
        ICommand[] commands = desc.getBuildSpec();
        for( int i = 0; i < commands.length; ++i ) {
          if( commands[ i ].getBuilderName().equals( AlexBuilder.BUILDER_ID ) ) {
            return;
          }
        }
        // add builder to project
        ICommand command = desc.newCommand();
        command.setBuilderName( AlexBuilder.BUILDER_ID );
        ICommand[] nc = new ICommand[ commands.length + 1 ];
        // Add it before other builders.
        System.arraycopy( commands, 0, nc, 1, commands.length );
        nc[ 0 ] = command;
        desc.setBuildSpec( nc );
        project.setDescription( desc, null );
      } catch( CoreException e ) {
        HaskellCorePlugin.log( e );
      }
    }
  }

  @Override
  public void selectionChanged( final IAction action, final ISelection selection ) {
    Collection<IProject> prjs = ResourceUtil.getProjects( selection );
    if( prjs.size() > 0 ) {
      project = prjs.iterator().next();
    }
  }

  @Override
  public void setActivePart( final IAction action,
      final IWorkbenchPart targetPart ) {
    // currentShell = targetPart.getSite().getShell();
  }
}
