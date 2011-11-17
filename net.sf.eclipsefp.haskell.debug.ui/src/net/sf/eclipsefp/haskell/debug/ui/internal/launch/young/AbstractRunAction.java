/**
 *
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.ProjectExplorerStanza;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;


/**
 * @author Alejandro Serrano
 *
 */
public abstract class AbstractRunAction extends Action {

  protected final ISelectionProvider selectionProvider;
  protected ProjectExplorerStanza stanza;
  protected IProject project;

  protected AbstractRunAction( final String title,
      final ISelectionProvider selProvider ) {
    super( title );
    this.selectionProvider = selProvider;
  }

  protected abstract CabalSyntax getTargetSection();

  protected abstract String getLaunchConfigName();

  protected abstract ILaunchConfigurationWorkingCopy createLaunchConfig()
      throws CoreException;

  protected abstract String getLaunchMode();

  @Override
  public boolean isEnabled() {
    ISelection selection = selectionProvider.getSelection();
    stanza = null;
    if( selection != null && !selection.isEmpty() ) {
      IStructuredSelection ss = ( ( IStructuredSelection )selection );
      if( ss.size() == 1 ) {
        Object o = ss.getFirstElement();
        if( o instanceof ProjectExplorerStanza ) {
          stanza = ( ProjectExplorerStanza )o;
          project = stanza.getOwner().getProject();
        }
      }
    }
    return stanza != null
        && stanza.getStanza().getType().equals( getTargetSection() );
  }

  @Override
  public void run() {
    try {
      ILaunchConfigurationType type = LaunchOperation
          .getConfigType( getLaunchConfigName() );
      List<ILaunchConfiguration> launches = LaunchOperation
          .getConfigurationsForProject( type, project.getName() );
      ILaunchConfiguration launch = null;
      if( launches.size() > 0 ) {
        launch = launches.get( 0 );
      } else {
        // Create the launch configuration
        ILaunchConfigurationWorkingCopy wc = createLaunchConfig();
        wc.doSave();
        launch = wc;
      }
      launch.launch( getLaunchMode(), null );
    } catch( Exception e ) {
      // Do nothing
    }
  }
}
