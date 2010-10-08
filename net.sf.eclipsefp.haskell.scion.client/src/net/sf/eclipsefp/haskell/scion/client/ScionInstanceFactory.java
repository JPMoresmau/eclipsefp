package net.sf.eclipsefp.haskell.scion.client;

import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import net.sf.eclipsefp.haskell.scion.client.nls.ScionText;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.util.NullWriter;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;

public class ScionInstanceFactory  {
  /** Scion server event listeners */
  private final ListenerList evListeners;
  /** The project -> scion instance map */
  private final Map<IProject, ScionInstanceState> instances;
  /** Configuration is ? */
  private boolean configurationValid;
  /** Use the built-in scion-server */
  private boolean useBuiltIn;
  /** The current executable's path */
  private IPath executablePath;
  /** The project-less instance, used (primarily) for lexical analysis and syntax highlighting */
  ScionInstanceState sharedInstance;

  /** Singleton instance container class for the factory */
  private static class FactoryInstanceContainer {
    public final static ScionInstanceFactory theInstance = new ScionInstanceFactory();
  }

  /** The built-in scion server's path */
  private static class BuiltInScionServerPath {
    public final static IPath thePath = ScionPlugin.defaultServerExecutablePath();
  }

  /** Default, hidden constructor used to create the singleton instance */
  private ScionInstanceFactory() {
    evListeners = new ListenerList(ListenerList.EQUALITY);
    instances = new HashMap<IProject, ScionInstanceState>();
    sharedInstance = new ScionInstanceState(NullScionInstance.getDefaultInstance(), new NullWriter());

    // Configuration variables' defaults
    configurationValid = false;
    useBuiltIn = true;
    executablePath = null;
  }

  /** Get the factory's instance */
  public static ScionInstanceFactory getFactory() {
    return FactoryInstanceContainer.theInstance;
  }
  
  /** Get the shared instance */
  public static IScionInstance getSharedScionInstance() {
    ScionInstanceFactory theFactory = getFactory();
    IScionInstance retval = null;
    
    if (theFactory.configurationValid) {
      if (theFactory.sharedInstance != null) {
        retval = theFactory.sharedInstance.getInstance();
      }
    }
    
    return retval;
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Configuration variable getters and setters
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  /** Set the factory's configuration
   *
   * This method sanity checks the configuration variables passed into the method, arranges
   * for a built-in server build if necessary.
   */
  public synchronized void setConfiguration(final boolean useBuiltIn, final IPath executablePath ) {
    configurationValid = (useBuiltIn || !executablePath.isEmpty());
    if (configurationValid) {
      this.useBuiltIn = useBuiltIn;
      this.executablePath = executablePath;
      processConfigurationChange();
    }
  }

  /** Reset configuration back to an invalid state. */
  public synchronized void setInvalidConfiguration() {
    configurationValid = false;
    useBuiltIn = true;
    executablePath = null;
  }

  /** Is configuration actually valid? */
  public final boolean isConfigurationValid() {
    return configurationValid;
  }
  
  /** Actions to be done when the configuration changes */
  public synchronized void processConfigurationChange() {
    if (useBuiltIn) {
      if ( builtinNeedsRebuild() ) {
        // NOTE: Moving all of the machinery required to do the rebuild to this module
        // introduces a module cycle, hence the indirect signal back to haskell.ui to
        // cause the rebuild to occur.
        notifyListeners(NullScionInstance.getDefaultInstance(), ScionServerEventType.NEEDS_REBUILD);
      } else {
        serverExecutableChanged( BuiltInScionServerPath.thePath );
      }
    } else {
      serverExecutableChanged(executablePath);
    }
  }
  
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  /** Create a new scion-server instance
   *
   * @param project
   */
  private ScionInstanceState createScionServerInstance( final IProject project, final String consoleName ) {
    IConsoleManager consoleMgr = ConsolePlugin.getDefault().getConsoleManager();
    IOConsole console = null;
    
    for (IConsole cons : consoleMgr.getConsoles()) {
      if (cons.getName().equals(consoleName)) {
        console = (IOConsole) cons;
        break;
      }
    }
    
    if (console == null) {
      console = new IOConsole(consoleName, null);
      consoleMgr.addConsoles(new IConsole[] { console });
    }

    final IOConsoleOutputStream stream = console.newOutputStream();
    final Display display = getStandardDisplay();
    display.syncExec( new Runnable() {
      public void run() {
        // Because JP likes blue for logging
        stream.setColor(display.getSystemColor(SWT.COLOR_BLUE));
      }
    });

    Writer streamWriter = new OutputStreamWriter (stream);
    IPath serverPath = executablePath;
    if (useBuiltIn) {
      serverPath = BuiltInScionServerPath.thePath;
    }

    IScionInstance instance;
    if (configurationValid) {
      instance = new ScionInstance(serverPath, project, streamWriter);
    } else {
      instance = new NullScionInstance(serverPath, project, streamWriter);
    }
    
    return new ScionInstanceState(instance, streamWriter);
  }

  public IScionInstance createScionServerInstance (final IProject project ) {
    ScionInstanceState state = createScionServerInstance( project, ScionInstanceFactory.consoleName(project) );
    instances.put( project, state );
    return state.getInstance();
  }
  
  public ScionInstanceState recreateScionServerInstance( final ScionInstanceState src, final IProject project,
      final IPath serverExecutable )
      throws ScionServerStartupException {
    IScionInstance instance = src.getInstance();
    
    instance.stop();
    if (configurationValid) {
      instance = new ScionInstance(serverExecutable, project, src.getStreamWriter());
    } else {
      instance = new NullScionInstance(serverExecutable, project, src.getStreamWriter());
    }
    instance.start();
    return new ScionInstanceState(instance, src.getStreamWriter());
  }

  private ScionInstanceState createSharedServerInstance() {
    return createScionServerInstance( null, ScionText.sharedLexerScionInstance);
  }

  /** Get the default, standard display for a console */
  private Display getStandardDisplay() {
    Display display = Display.getCurrent();
    if( display == null ) {
      display = Display.getDefault();
    }
    return display;
  }

  /** Add a new server event listener */
  public void addListener(final IScionServerEventListener listener) {
    evListeners.add(listener);
  }

  /** Remove a server event listener */
  public void removeListener(final IScionServerEventListener listener) {
    evListeners.remove(listener);
  }

  /** Fire off a server event, notify all listeners */
  private void notifyListeners(final IScionInstance serverInstance, final ScionServerEventType evType) {
    Object[] listeners = evListeners.getListeners();
    ScionServerEvent event = new ScionServerEvent(serverInstance, evType);
    for (int i = 0; i < listeners.length; ++i) {
           ((IScionServerEventListener) listeners[i]).processScionServerEvent(event);
    }
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Instance management:
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  /** Stop all scion-server instances */
  public void stopAllInstances() {
    for( IProject project: instances.keySet() ) {
      stopInstance( instances.get( project ).getInstance() );
    }
    instances.clear();
  }

  /** Stop an instance referenced by a resource's project */
  public void stopInstance(final IResource res){
    stopInstance(res.getProject());
  }

  /** Stop an instance referenced by a project */
  public void stopInstance( final IProject project ) {
    ScionInstanceState state = instances.remove( project );
    IScionInstance instance = state.getInstance();
    if (instance != null){
      stopInstance(instance);
      notifyListeners(instance, ScionServerEventType.TERMINATED);
    }
  }

  /**
   * Stops the Scion instance for the given project. Does not remove the
   * instance from the instances map.
   */
  private void stopInstance( final IScionInstance instance ) {
    if( instance == null ) {
      return;
    }

    instance.stop();
    IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
    IProject project = instance.getProject();
    String name = consoleName( project);
    for( IConsole c : mgr.getConsoles() ) {
      if( c.getName().equals( name ) ) {
        mgr.removeConsoles( new IConsole[] { c } );
        break;
      }
    }
  }

  /** Create a console name string using the project name, if available.
   * @param project The project
   * @return A console name
   */
  public static final String consoleName ( final IProject project ) {
    String projectName = project != null ? project.getName() : ScionText.noproject;
    return NLS.bind( ScionText.scion_console_title, projectName );
  }

  /**
   * Called when the the server executable path has changed, causing a restart of all instances.
   * This method is synchronized to avoid concurrent modifications.
   */
  public synchronized void serverExecutableChanged(final IPath serverExecutable) {
    ScionServerStartupException exception = null;
    
    if (!useBuiltIn) {
      this.executablePath = serverExecutable;
    }

    // Update the shared instance:
    try {
      IScionInstance instance;
      ScionServerEventType evType = ScionServerEventType.EXECUTABLE_CHANGED;

      if ( sharedInstance == null ) {
        evType = ScionServerEventType.VALID_RUNNING;
      } else {
        sharedInstance.getInstance().stop();
      }
      sharedInstance = createSharedServerInstance();
      sharedInstance.getInstance().start();
      notifyListeners(sharedInstance.getInstance(), evType);

      for (IProject project : new ArrayList<IProject>(instances.keySet())) {
        ScionInstanceState state = instances.get(project);
        instance = state.getInstance();
        evType = ScionServerEventType.EXECUTABLE_CHANGED;
        if (instance == null) {
          evType = ScionServerEventType.VALID_RUNNING;
        } else {
          instance.stop();
        }
        // recreate the scion-server
        instance = createScionServerInstance(project);
        instance.start();
        notifyListeners(instance, evType);
      }
    } catch( ScionServerStartupException ex ) {
      exception = ex;
    }

    if( exception != null ) {
      // we want to bug the user about this just once, not once for every
      // project
      notifyListeners(NullScionInstance.getDefaultInstance(), ScionServerEventType.NOT_STARTED);
    }
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Accessors and predicates
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  /** Get the scion-server instance corresponding to a particular file */
  public IScionInstance getScionInstance( final IFile file ) {
    return getScionInstance(file.getProject());
  }

  /** Get the scion-server instance corresponding to a particular resource */
  public IScionInstance getScionInstance( final IResource res ) {
    return getScionInstance(res.getProject());
  }

  /** Get the scion-server instance corresponding to a particular project */
  public IScionInstance getScionInstance( final IProject project ) {
    return instances.get(project).getInstance();
  }

  /** Does a project have an associated scion-server? */
  public boolean hasScionInstance( final IProject project ) {
    return instances.containsKey(project);
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Built-in scion-server management
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  /** Does the built-in scion server need rebuilding? */
  private boolean builtinNeedsRebuild() {
    return (useBuiltIn && !ScionPlugin.serverExecutablePath( ScionPlugin.builtinServerDirectoryPath() ).toFile().exists());
  }

  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Internal map value class: we'd like to keep more than just the scion server
  // instance...
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  private class ScionInstanceState {
    /** The scion-server instance */
    IScionInstance instance;
    /** The output writer stream */
    Writer streamWriter;

    /** The constructor */
    public ScionInstanceState( final IScionInstance instance, final Writer outWriter ) {
      this.instance = instance;
      this.streamWriter = outWriter;
    }

    /** Get the scion instance */
    public IScionInstance getInstance() {
      return instance;
    }

    /** Get the output writer stream */
    public Writer getStreamWriter() {
      return streamWriter;
    }
  }
}
