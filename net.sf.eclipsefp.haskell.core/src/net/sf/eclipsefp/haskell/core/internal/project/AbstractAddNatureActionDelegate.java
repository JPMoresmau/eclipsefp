package net.sf.eclipsefp.haskell.core.internal.project;

import java.util.Collection;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.WorkspaceModifyOperation;

/**
 * Abstract base class for actions that add a nature to a project
 * @author JP Moresmau
 *
 */
public abstract class AbstractAddNatureActionDelegate  implements IObjectActionDelegate {

    // private Shell currentShell;
    private IProject project;

    public AbstractAddNatureActionDelegate() {
      // Do nothing
    }

    protected abstract String getBuilderID();

    public void run( final IAction action ) {
      if( project != null ) {
        IProgressMonitor mon = new NullProgressMonitor();
        WorkspaceModifyOperation wmo = new WorkspaceModifyOperation(ResourcesPlugin.getWorkspace().getRoot()) {
          @Override
          protected void execute(final IProgressMonitor monitor) throws CoreException {
              IProjectDescription desc = project.getDescription();
              ICommand[] commands = desc.getBuildSpec();
              for( int i = 0; i < commands.length; ++i ) {
                if( commands[ i ].getBuilderName().equals( getBuilderID() ) ) {
                  return;
                }
              }
              // add builder to project
              ICommand command = desc.newCommand();
              command.setBuilderName( getBuilderID() );
              ICommand[] nc = new ICommand[ commands.length + 1 ];
              // Add it before other builders.
              System.arraycopy( commands, 0, nc, 1, commands.length );
              nc[ 0 ] = command;
              desc.setBuildSpec( nc );
              project.setDescription( desc, null );
          }
        };
        try {
         // ResourcesPlugin.getWorkspace().run( operation, mon );
            wmo.run( mon );
        } catch( Exception cex ) {
          HaskellCorePlugin.log( NLS.bind( CoreTexts.projectNatureAddOperation_error, getBuilderID() ), cex );
        } finally {
          mon.done();
        }
      }
    }

    public void selectionChanged( final IAction action, final ISelection selection ) {
      Collection<IProject> prjs = ResourceUtil.getProjects( selection );
      if( prjs.size() > 0 ) {
        project = prjs.iterator().next();
      }
    }

    public void setActivePart( final IAction action,
        final IWorkbenchPart targetPart ) {
      // NOOP
    }
}
