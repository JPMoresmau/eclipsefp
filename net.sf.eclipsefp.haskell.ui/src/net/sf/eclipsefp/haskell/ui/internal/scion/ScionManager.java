package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.util.Map;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.console.HaskellConsole;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ScionPP;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.statushandlers.StatusManager;

/**
 * Manages instances of Scion servers.
 *
 * This class ensures that there is exactly one running Scion instance for each
 * open project. This instance can be accessed through
 * {@link #getScionInstance(IResource)}.
 *
 * This works by listening for resource changes.
 */
public class ScionManager implements IResourceChangeListener {

  private String serverExecutable = null;

  /**
   * Used to alert the user of Scion startup failure only once per session.
   */
  private boolean serverStartupErrorReported = true; // TODO TtC set back to false

  private final Map<IProject, ScionInstance> instances = ScionPlugin.getDefault().getScionInstances();


  public ScionManager() {
    // the work is done in the start() method
  }

  public void start() {
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    serverExecutable = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_EXECUTABLE );
    preferenceStore.addPropertyChangeListener( new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        if (event.getProperty().equals( IPreferenceConstants.SCION_SERVER_EXECUTABLE )) {
          if (event.getNewValue() instanceof String && !((String)event.getNewValue()).equals( serverExecutable )) {
            serverExecutable = (String) event.getNewValue();
            serverExecutableChanged();
          }
        }
      }
    });

    try {
      ResourcesPlugin.getWorkspace().getRoot().accept( new IResourceVisitor() {

        public boolean visit( final IResource resource ) throws CoreException {
          return updateForResource( resource );
        }
      } );
    } catch( CoreException ex ) {
      HaskellUIPlugin.log(
          "Error when processing resource delta from ScionManager", ex );
    }

    ResourcesPlugin.getWorkspace().addResourceChangeListener( this,
        IResourceChangeEvent.POST_CHANGE );
  }

  public void stop() {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );

    for( IProject project: instances.keySet() ) {
      stopInstance( instances.get(project) );
    }
    instances.clear();
  }

  /**
   * Returns the instance manager for the given resource. The resource must be
   * part of a currently opened project.
   */
  public ScionInstance getScionInstance( final IResource resource ) {
    IProject project = resource.getProject();
    if( instances.containsKey( project ) ) {
      return instances.get( project );
    }
    return null;
  }

  /**
   * Called after a resource in the workspace was changed. It finds all projects
   * that were opened/closed, and starts/stops Scion instances accordingly.
   */
  public void resourceChanged( final IResourceChangeEvent event ) {
    try {
      event.getDelta().accept( new IResourceDeltaVisitor() {

        public boolean visit( final IResourceDelta delta ) throws CoreException {
          return updateForResource( delta.getResource() );
        }
      } );
    } catch( CoreException ex ) {
      HaskellUIPlugin.log(
          "Error when processing resource delta from ScionManager", ex );
    }
  }

  /**
   * Called when the preference value for the server executable path has changed.
   * We restart all instances.
   */
  private void serverExecutableChanged() {
    ScionServerStartupException exception = null;
    for (IProject project : instances.keySet()) {
      try {
        instances.get( project ).setServerExecutable( serverExecutable );
      } catch (ScionServerStartupException ex) {
        exception = ex;
      }
    }
    if (exception != null) {
      // we want to bug the user about this just once, not once for every project
      reportServerStartupError(exception);
    }
  }

  private boolean updateForResource( final IResource resource ) throws CoreException {
    if( resource instanceof IProject ) {
      IProject project = ( IProject )resource;
      if( project.isOpen() && !instances.containsKey( project ) && project.hasNature( HaskellNature.NATURE_ID ) ) {
        ScionInstance instance = startInstance( project );
        instances.put( project, instance );
      }
      if( !project.isOpen() && instances.containsKey( project ) ) {
        // we cannot check the nature of closed projects, but if it's in instances, stop it
        stopInstance( instances.get(project) );
        instances.remove( project );
      }
      return false; // projects can't be children of other projects, can they?
    }
    return true;
  }

  /**
   * Starts and returns a new Scion instance for the given project.
   * Does not add the instance to the instances map.
   */
  private synchronized ScionInstance startInstance( final IProject project ) {
	String name=NLS.bind( UITexts.scion_console_title, project.getName() );
	HaskellConsole c=new HaskellConsole( null, name );

    ScionInstance instance = new ScionInstance( serverExecutable,project,c.createOutputWriter() );
    try {
      instance.start();
    } catch (ScionServerStartupException ex) {
      reportServerStartupError(ex);
    }
    return instance;
  }

  /**
   * Stops the Scion instance for the given project.
   * Does not remove the instance from the instances map.
   */
  private void stopInstance( final ScionInstance instance ) {
	instance.stop();
	IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
	String name=NLS.bind( UITexts.scion_console_title, instance.getProject().getName() );
	for (IConsole c:mgr.getConsoles()){
		if (c.getName().equals(name)){
			mgr.removeConsoles( new IConsole[]{c} );
			break;
		}
	}
  }

  private void reportServerStartupError(final ScionServerStartupException ex) {
    if (!serverStartupErrorReported) {
      IStatus status = new Status(IStatus.ERROR, HaskellUIPlugin.getPluginId(), ex.getMessage(), ex);
      StatusManager.getManager().handle( status, StatusManager.LOG );
      HaskellUIPlugin.getStandardDisplay().asyncExec( new Runnable() {
        public void run() {
          Shell parent = HaskellUIPlugin.getStandardDisplay().getActiveShell();
          String text = NLS.bind( UITexts.scionServerStartupError_message, ScionPP.getServerExecutableName() );
          if ( MessageDialog.openQuestion( parent, UITexts.scionServerStartupError_title, text ) ) {
            PreferenceDialog prefDialog = PreferencesUtil.createPreferenceDialogOn( parent, ScionPP.PAGE_ID, null, null );
            prefDialog.open();
          }
        }
      });
      serverStartupErrorReported = true;
    }
  }

}
