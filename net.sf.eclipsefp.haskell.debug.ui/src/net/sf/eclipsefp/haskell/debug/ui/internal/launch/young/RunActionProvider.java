package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.BaseExecutableLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.ILaunchAttributes;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.ProjectExplorerStanza;
import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonMenuConstants;
import org.eclipse.ui.navigator.ICommonViewerSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;


public class RunActionProvider extends CommonActionProvider {

  private RunComponentAction runAction;

  @Override
  public void init( final ICommonActionExtensionSite aSite ) {
    super.init( aSite );
    ICommonViewerSite viewSite = aSite.getViewSite();
    if( viewSite instanceof ICommonViewerWorkbenchSite ) {
      ICommonViewerWorkbenchSite wSite = ( ICommonViewerWorkbenchSite )viewSite;
      runAction = new RunComponentAction( wSite.getSelectionProvider() );
    }
  }

  @Override
  public void fillContextMenu( final IMenuManager menu ) {
    if( runAction != null && runAction.isEnabled() ) {
      menu.appendToGroup( ICommonMenuConstants.GROUP_OPEN, runAction );
    }
  }

  private static class RunComponentAction extends Action {

    private final ISelectionProvider selectionProvider;
    private ProjectExplorerStanza stanza;
    private IProject project;

    private RunComponentAction( final ISelectionProvider selProvider ) {
      super( UITexts.runExecutable );
      this.selectionProvider = selProvider;
    }

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
      return stanza != null && stanza.getStanza().getType().equals( CabalSyntax.SECTION_EXECUTABLE );
    }

    @Override
    public void run() {
      try {
        ILaunchConfigurationType type = LaunchOperation
            .getConfigType( BaseExecutableLaunchDelegate.class.getName() );
        List<ILaunchConfiguration> launches = LaunchOperation
            .getConfigurationsForProject( type, project.getName() );
        ILaunchConfiguration launch = null;
        if( launches.size() > 0 ) {
          launch = launches.get( 0 );
        } else {
          // Create the launch configuration
          String id = LaunchOperation.createConfigId( project.getName() + "/" + stanza.getStanza().getName() ); //$NON-NLS-1$
          ILaunchConfigurationWorkingCopy wc = type.newInstance( null, id );
          wc.setAttribute( ILaunchAttributes.PROJECT_NAME, project.getName() );
          wc.setAttribute( ILaunchAttributes.STANZA, stanza.getStanza().getName() );
          wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, project.getLocation().toOSString() );
          wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
          wc.doSave();
          launch = wc;
        }
        launch.launch( ILaunchManager.RUN_MODE, null );
      } catch( Exception e ) {
        // Do nothing
      }
    }
  }

}
