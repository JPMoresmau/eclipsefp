/**
 *
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.debug.ui.internal.HaskellDebugUI;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.ProjectExplorerStanza;
import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.plugin.AbstractUIPlugin;


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
    super( title ,AbstractUIPlugin.imageDescriptorFromPlugin( HaskellDebugUI.getDefault().getBundle().getSymbolicName(), "icons/etool16/hsexe16.gif" ) ); //$NON-NLS-1$
    this.selectionProvider = selProvider;
  }

  protected abstract CabalSyntax getTargetSection();

  protected String getLaunchMode() {
    return ILaunchManager.RUN_MODE;
  }

 /* protected abstract String getLaunchConfigName();

  protected abstract ILaunchConfigurationWorkingCopy createLaunchConfig()
      throws CoreException;

  protected abstract String getLaunchMode();
*/
  protected abstract ILaunchShortcut2 getShortcut();

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
      /*ILaunchConfigurationType type = LaunchOperation
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
      launch.launch( getLaunchMode(), null );*/
      getShortcut().launch( new StructuredSelection(stanza), getLaunchMode() );
    } catch( Exception e ) {
      // Do nothing
    }
  }
}
