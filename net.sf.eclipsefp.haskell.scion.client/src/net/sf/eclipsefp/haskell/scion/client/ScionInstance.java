package net.sf.eclipsefp.haskell.scion.client;

import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckArbitraryCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckFileCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.CabalDependenciesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.CompilationResultHandler;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.DefinedNamesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListCabalComponentsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListExposedModulesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.LoadCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ModuleGraphCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.OutlineCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ParseCabalCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.SetVerbosityCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.TokenTypesCommand;
import net.sf.eclipsefp.haskell.scion.internal.servers.NullScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.GhcMessages;
import net.sf.eclipsefp.haskell.scion.types.IOutlineHandler;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;
import net.sf.eclipsefp.haskell.scion.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.util.FileUtil;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SafeRunner;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.osgi.util.NLS;
import org.json.JSONObject;

/**
 * Manages a single instance of the Scion server.
 * 
 * Objects from this class keep track of the state of the Scion server, so that
 * the server can be put into the same state after a restart (either of the
 * server, or of the entire workbench).
 * 
 * @author Thomas ten Cate
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class ScionInstance {
  /** The scion-server with whom this object communicates */
  private ScionServer                 server;
  /** Server running flag */
  private boolean                     serverRunning;
  /** The project with which the scion-server is associated */
  private IProject                    project;
  /** The currently loaded file */
  private IFile                       loadedFile;

  private JSONObject                  cabalDescription;
  private Map<String, CabalPackage[]> packagesByDB;
  private List<Component>             components;
  private CabalComponentResolver      resolver;
  private Component                   lastLoadedComponent;
  private List<String>                exposedModulesCache;

  private Map<IFile, LoadInfo>        loadInfos;

  /** The listener list for objects interested in server status events */
  private final ListenerList          listeners;

  /**
   * The constructor
   * 
   * @param server
   *          The scion-server instance to whom commands are sent
   * @param project
   *          The associated {@link IProject IProject}
   * @param resolver
   *          The Cabal component resolver
   */
  public ScionInstance(ScionServer server, IProject project, CabalComponentResolver resolver) {
    this.server = server;
    this.serverRunning = false;
    this.project = project;
    this.resolver = resolver;
    
    this.lastLoadedComponent = null;
    this.loadedFile = null;
    this.components = new LinkedList<Component>();
    this.exposedModulesCache = null;
    this.loadInfos = new HashMap<IFile, LoadInfo>();
    this.listeners = new ListenerList();
    this.packagesByDB = null;
  }

  /** Get the project associated with this instance */
  public final IProject getProject() {
    return project;
  }

  // -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Listener management
  // -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

  /** Add a scion-server event change listener */
  public void addListener(IScionEventListener listener) {
    listeners.add(listener);
  }

  /** Remove a scion-server event change listener */
  public void removeListener(IScionEventListener listener) {
    listeners.remove(listener);
  }

  /**
   * Notify listeners that a scion-server event occurred.
   * 
   * @param evType
   *          The type of event that just happened.
   */
  public void notifyListeners(ScionEventType evType) {
    notifyListeners(new ScionEvent(this, server, evType));
  }

  /**
   * Notify listeners that a scion-server event occurred.
   * 
   * @param ev
   *          The event that just happened.
   */
  public void notifyListeners(final ScionEvent ev) {
    Object[] theListeners = listeners.getListeners();
    for (int i = 0; i < theListeners.length; ++i) {
      final IScionEventListener receiver = (IScionEventListener) theListeners[i];
      SafeRunner.run(new SafeRunnable("Scion-server event listener") { //$NON-NLS-1$
        public void run() {
          receiver.processScionServerEvent(ev);
        }
      } );
    }
  }

  /**
   * Update the scion-server executable. This method is invoked in response to a
   * preference change in the UI to signal that the underlying scion-server has
   * changed. Alternatively, this method gets triggered during the UI's startup
   * when EclipseFP changes from the {@link NullScionServer} default server to
   * a real server after preferences, etc., are read or when the built-in server's
   * recompile finishes successfully.
   * 
   * @param server
   *          The scion-server executable that this instance should use.
   * @throws ScionServerStartupException
   *           if the server could not be started.
   */
  public void setServerExecutable(final ScionServer newServer) throws ScionServerStartupException {
    if (!server.equals(newServer)) {
      stop(true);
      server = newServer;
      start();
      notifyListeners(ScionEventType.EXECUTABLE_CHANGED);
    }
  }

  /** Redirect the underlying server's output stream to a new Writer. */
  public void setOutputStream(final Writer outStream) {
    server.setOutputStream(outStream);
  }

  /** Start the instance's underlying server */
  public void start() throws ScionServerStartupException {
    // This internalReset() is a defensive programming move: ensure all state is cleared out before starting the
    // scion-server.
    internalReset();
    
    server.startServer();
    serverRunning = true;
    checkProtocol();

    if (Trace.isTracing()) {
      setDeafening();
    }
    
    // And finally set up the initial project build...
    Job projectJob = buildProject(false, true);
    
    if (projectJob != null)
      projectJob.schedule();
  }

  /**
   * Stop the scion-server associated with this instance.
   * 
   * @param cleanly Flag passed on to the scion-server, determines if a quit command is sent.
   */
  public void stop(boolean cleanly) {
    internalReset();
    assert(server != null);
    server.stopServer(cleanly);
    serverRunning = false;
  }
  
  /** Is the server still running? */
  public boolean isStopped() {
    return !serverRunning;
  }

  /**
   * Check the server's protocol version. This logs a warning if the
   * version numbers do not match; a {@link VersionMismatchEvent} event
   * is fired off to server event listeners.
   */
  public void checkProtocol() {
    final ConnectionInfoCommand ciCmd = new ConnectionInfoCommand();
    
    ciCmd.addContinuation(new Job("Checking protocol version") {
      @Override
      protected IStatus run(IProgressMonitor monitor) {
        int version = ciCmd.getVersion();

        if (version != ScionServer.WIRE_PROTOCOL_VERSION) {
          final String errMsg = NLS.bind(ScionText.commandVersionMismatch_warning, version, ScionServer.WIRE_PROTOCOL_VERSION);
          ScionPlugin.logWarning(errMsg, null);
          notifyListeners(new VersionMismatchEvent(ScionInstance.this, server, version));
        }
        
        return Status.OK_STATUS;
      }
    } );
    
    server.queueCommand(ciCmd);
  }

  /**
   * Predicate that ensures that the cabal file exists.
   * 
   * @return true if the cabal file exists.
   */
  private boolean checkCabalFile() {
    if (getProject() == null) {
      return false;
    }
    IFile cabalFile = getCabalFile(getProject());
    boolean exists = cabalFile.exists();
    if (!exists) {
      String msg = ScionText.bind(ScionText.cabalFileMissing, cabalFile.getLocation().toString());
      ScionPlugin.logError(msg, null);
      if (!getProject().getWorkspace().isTreeLocked()) {
        String id = ScionPlugin.ID_PROJECT_PROBLEM_MARKER;
        try {
          IMarker marker = getProject().createMarker(id);
          marker.setAttribute(IMarker.MESSAGE, msg);
          marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
        } catch (CoreException ce) {
          ScionPlugin.logError(msg, ce);
        }
      }
    }
    
    return exists;
  }

  /**
   * Build the Haskell project.
   * 
   * @param output
   *          Echo output from the build
   * @param forceRecomp
   *          Force recompilation if true
   * @return TODO
   */
  public Job buildProject(final boolean output, final boolean forceRecomp) {
    ProjectCommandGroup retval = null;
    
    if (checkCabalFile()) {
      final String jobNamePrefix = NLS.bind(ScionText.build_job_name, getProject().getName());

      retval = new ProjectCommandGroup(jobNamePrefix, getProject()) {
        @Override
        protected IStatus run(IProgressMonitor monitor) {
          try {
            monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
            buildProjectInternal(monitor, output, forceRecomp);
          } finally {
            monitor.done();
          }
          return Status.OK_STATUS;
        }
      };
      
      retval.setPriority(Job.BUILD);
    }
    
    return retval;
  }
  
  public boolean buildProjectWithinJob(final IProgressMonitor monitor, final boolean output, final boolean forceRecomp) {
    boolean retval = false;
    IJobManager mgr = Job.getJobManager();
    
    try {
      mgr.beginRule(project, monitor);
      if ( checkCabalFile() ) {
        retval = buildProjectInternal(monitor, output, forceRecomp);
      }
    } finally {
      mgr.endRule(project);
    }
    
    return retval;
  }
  
  private boolean buildProjectInternal(final IProgressMonitor monitor, final boolean output, final boolean forceRecomp) {
    boolean retval = false;
    final String projectName = project.getName();
    
    if ( listComponents(monitor) && loadComponents(monitor, output, forceRecomp) ) {
      monitor.subTask( NLS.bind( ScionText.buildProject_parseCabalDescription, projectName ) );
      ParseCabalCommand pcc = new ParseCabalCommand(getCabalFile(getProject()).getLocation().toOSString());
      if (server.sendCommand(pcc)) {
        cabalDescription = pcc.getDescription();

        monitor.subTask( NLS.bind( ScionText.buildProject_cabalDependencies, projectName ) );
        CabalDependenciesCommand cdc = new CabalDependenciesCommand(getCabalFile(getProject()).getLocation().toOSString());
        if (server.sendCommand(cdc)) {
          packagesByDB = cdc.getPackagesByDB();
          retval = true;
          restoreState();
        }
      }
    }
    
    return retval;
  }

  private boolean listComponents(IProgressMonitor monitor) {
    monitor.subTask( NLS.bind( ScionText.buildProject_listComponents, project.getName() ) );

    cabalDescription = null;

    final String cabalProjectFile = getCabalFile(getProject()).getLocation().toOSString();
    final ListCabalComponentsCommand command = new ListCabalComponentsCommand(cabalProjectFile);
    
    if (server.sendCommand(command)) {
      components = command.getComponents();

      // if lastLoadedComponent is still present, load it last
      if (lastLoadedComponent != null) {
        List<Component> l = new ArrayList<Component>(components.size());
        Component toLoadLast = null;
        synchronized (components) {
          for (Component c : components) {
            if (c.toString().equals(lastLoadedComponent.toString())) {
              toLoadLast = c;
            } else {
              l.add(c);
            }
          }
        }
  
        if (toLoadLast != null) {
          l.add(toLoadLast);
        }
        
        components = l;
      }
      
      return true;
    }
    
    return false;
  }
  
  private boolean loadComponents(IProgressMonitor monitor, final boolean output, final boolean forceRecomp) {
    List<Component> cs = null;
    IProject theProject = getProject();
    boolean failed = false;
    
    synchronized (components) {
      cs = new ArrayList<Component>(components);
    }
    
    deleteProblems(theProject);
    final CompilationResultHandler crh = new CompilationResultHandler(theProject);

    for (Component c : cs) {
      monitor.subTask( NLS.bind ( ScionText.buildProject_loadComponents, c.toString() ) );

      final LoadCommand loadCommand = new LoadCommand(theProject, c, output, forceRecomp);
      if (server.sendCommand(loadCommand)) {
        crh.process(loadCommand);
        lastLoadedComponent = c;
      } else {
        failed = true;
        break;
      }
    }
      
    if (failed) {
      // Clear out state
      internalReset();
    }
    
    return !failed;
  }
  // ////////////////////
  // Internal commands

  /** Reset to initial state. */
  private void internalReset() {
    cabalDescription = null;
    synchronized (components) {
      components.clear();
    }

    exposedModulesCache = null;
    lastLoadedComponent = null;
    loadedFile = null;
    loadInfos.clear();
    packagesByDB = null;
  }
  
  private void restoreState() {
    if (loadedFile != null) {
      // Ensure that loadedFile is really the loaded file.	
      loadFile( loadedFile );
    }
  }

  // ////////////////////
  // External commands

  public void loadFile(IFile fileName) {
    reloadFile( fileName );
  }

  public IFile getLoadedFile() {
    return loadedFile;
  }

  public boolean isLoaded(IFile f) {
    return f.equals(loadedFile);
  }

  public void setLoadedFile(IFile loadedFile) {
    this.loadedFile = loadedFile;
  }

  public void deleteProblems(IResource r) {
    if (!r.getWorkspace().isTreeLocked() && r.exists() && r.getProject().isOpen()) {
      try {
        if (r instanceof IFile) {
          r.refreshLocal(IResource.DEPTH_ZERO, new NullProgressMonitor());
        }
        r.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
      } catch (CoreException ex) {
        ScionPlugin.logError(ScionText.error_deleteMarkers, ex);
        ex.printStackTrace();
      }
    }
  }

  /**
   * Ensure that the file's Cabal component is loaded in scion-server, setting
   * the correct context for subsequent commands.
   * 
   * @param file
   *          The file whose component is required.
   * @param nextCommand
   *          An optional command to send to the scion-server after the
   *          component is loaded.
   */
  private boolean runWithComponent( final IFile file ) {
    Set<String> componentNames = resolver.getComponents( file );
    boolean loadOK = true;
    
    if (lastLoadedComponent == null || !componentNames.contains(lastLoadedComponent.toString())) {
      Component toLoad = null;

      if (!componentNames.isEmpty()) {
        synchronized (components) {
          for (Component compo : components) {
            if (componentNames.contains(compo.toString())) {
              toLoad = compo;
              break;
            }
          }
        }
      }

      LoadInfo li = getLoadInfo(file);

      // we have no component: we create a file one
      if (toLoad == null) {
        toLoad = new Component(ComponentType.FILE, file.getName(), file.getLocation().toOSString());
        if (!li.useFileComponent) {
          li.useFileComponent = true;
          ScionPlugin.logWarning(ScionText.bind(ScionText.warning_file_component, file.getProjectRelativePath()), null);
        }
      } else {
        li.useFileComponent = false;
      }

      if (toLoad != null) {
        IProject fProject = file.getProject();
        // Project for this instance should be the same as the file's
        Assert.isTrue( getProject() == fProject );
        if ( server.sendCommand(new LoadCommand(fProject, toLoad, false, false)) ) {
          lastLoadedComponent = toLoad;
        } else {
          loadOK = false;
        }
      }
    }

    return loadOK;
  }

  /**
   * Reload a file into the scion-server, setting the proper context for
   * subsequent commands.
   * 
   * @param file
   *          The file to be loaded.
   */
  public boolean reloadFile(final IFile file) {
    return ( runWithComponent(file) && server.sendCommand(new BackgroundTypecheckFileCommand( this, file ) ) );
  }

  public boolean reloadFile(final IFile file, final IDocument doc) {
    final LoadInfo li = getLoadInfo(file);

    final BackgroundTypecheckArbitraryCommand cmd = new BackgroundTypecheckArbitraryCommand(this, file, doc) {
      @Override
      public boolean onError(String name, String message) {
        if (message != null && message.contains(GhcMessages.ERROR_INTERACTIVE_DISABLED)) {
          deleteProblems(file);
          if (!li.interactiveCheckDisabled) {
            final String errMsg = ScionText.bind(ScionText.warning_typecheck_arbitrary_failed, file.getProjectRelativePath(),
                message);
            ScionPlugin.logWarning(errMsg, null);
            li.interactiveCheckDisabled = true;
          }

          return true;
        }

        li.interactiveCheckDisabled = false;
        return super.onError(name, message);
      }
    };
    
    return runWithComponent( file ) && server.sendCommand( cmd );
  }

  public void unloadFile(IFile fileName) {
    Assert.isTrue( loadedFile.equals(fileName) );
    loadedFile = null;
  }

  public String thingAtPoint(final IDocument doc, Location location, boolean qualify, boolean typed) {
    // the scion command will only work fine if we have the proper file loaded
    final IFile file = location.getIFile(getProject());
    if (file != null) {
      final String jobName = NLS.bind(ScionText.thingatpoint_job_name, file.getName());
      final ThingAtPointCommand cmd = new ThingAtPointCommand(location, qualify, typed);
      new FileCommandGroup(jobName, file, Job.SHORT) {
        @Override
        protected IStatus run(final IProgressMonitor monitor) {
          reloadFile(file, doc);
          server.sendCommand(cmd);
          return Status.OK_STATUS;
        }
      }.runGroupSynchronously();
      return cmd.getThing();
    }
    return null;
  }

  public List<OutlineDef> outline(final IFile file) {
    final String jobName = NLS.bind(ScionText.outline_job_name, file.getName());
    final OutlineCommand cmd = new OutlineCommand(file);

    new FileCommandGroup(jobName, file, Job.SHORT) {
      @Override
      protected IStatus run(final IProgressMonitor monitor) {
        withLoadedFile(file, cmd);
        return Status.OK_STATUS;
      }
    }.runGroupSynchronously();
    
    return cmd.getOutlineDefs();
  }
  
  public void outline(final IFile file, final IDocument doc, final IOutlineHandler handler) {
    if (file != null && handler != null) {
      final String jobName = NLS.bind(ScionText.outline_job_name, file.getName());
      new FileCommandGroup(jobName, file, Job.SHORT) {
        @Override
        protected IStatus run(final IProgressMonitor monitor) {
          final OutlineCommand cmd = new OutlineCommand(file);

          reloadFile(file, doc);
          server.sendCommand(cmd);
          handler.handleOutline(cmd.getOutlineDefs());

          return Status.OK_STATUS;
        }
      }.schedule();
    }
  }

  private void withLoadedFile(final IFile file, final ScionCommand cmd) {
    if ( isLoaded(file) || reloadFile( file ) ) {
      server.sendCommand(cmd);
    }
  }

  public Location firstDefinitionLocation(String name) {
    NameDefinitionsCommand command = new NameDefinitionsCommand(name);
    server.sendCommand(command);
    return command.getFirstLocation();
  }

  public static IFile getCabalFile(final IProject p) {
    return p.getFile(new Path(p.getName()).addFileExtension(FileUtil.EXTENSION_CABAL));
  }

  public JSONObject getCabalDescription() {
    // Never called... :-)
    return cabalDescription;
  }

  public Map<String, CabalPackage[]> getPackagesByDB() {
    return packagesByDB;
  }

  public List<Component> getComponents() {
    return components;
  }

  public List<String> definedNames() {
    final DefinedNamesCommand command = new DefinedNamesCommand();
    server.sendCommand(command);
    return command.getNames();
  }

  public List<String> listExposedModules() {
    if (exposedModulesCache == null) {
      final ListExposedModulesCommand command = new ListExposedModulesCommand();

      server.sendCommand(command);
      exposedModulesCache = Collections.unmodifiableList(command.getNames());
    }
    
    return exposedModulesCache;
  }

  public List<String> moduleGraph() {
    final ModuleGraphCommand command = new ModuleGraphCommand();

    server.sendCommand(command);
    return command.getNames();
  }

  public synchronized List<TokenDef> tokenTypes(final IFile file, final String contents) {
    TokenTypesCommand command = new TokenTypesCommand(file, contents, FileUtil.hasLiterateExtension(file));
    server.sendCommand(command);
    return command.getTokens();
  }

  public void setDeafening() {
    server.sendCommand(new SetVerbosityCommand(3));
  }

  private synchronized LoadInfo getLoadInfo(IFile file) {
    LoadInfo li = loadInfos.get(file);
    if (li == null) {
      li = new LoadInfo();
      loadInfos.put(file, li);
    }
    return li;
  }

  private class LoadInfo {
    private boolean interactiveCheckDisabled = false;
    private boolean useFileComponent         = false;
  }
}
