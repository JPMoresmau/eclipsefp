package net.sf.eclipsefp.haskell.scion.client;

import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
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
import net.sf.eclipsefp.haskell.scion.internal.commands.CompletionTyCons;
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
import net.sf.eclipsefp.haskell.scion.internal.commands.TokenAtPoint;
import net.sf.eclipsefp.haskell.scion.internal.commands.TokenPrecedingPoint;
import net.sf.eclipsefp.haskell.scion.internal.commands.TokenTypesCommand;
import net.sf.eclipsefp.haskell.scion.internal.servers.NullScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.GhcMessages;
import net.sf.eclipsefp.haskell.scion.types.IAsyncScionCommandAction;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.OutlineDef;
import net.sf.eclipsefp.haskell.scion.types.OutlineHandler;
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
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.json.JSONObject;

/**
 * Manages a single instance of the Scion server.
 * 
 * Objects from this class keep track of the state of the Scion server, so that
 * the server can be put into the same state after a restart (either of the
 * server, or of the entire workbench).
 * 
 * @author Thomas ten Cate
 * @author B. Scott Michel (bscottm@ieee.org)
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
  /** The currently loaded file */
  private String                      loadedDocContents;
  
  /** Flag that indicates that the project has been built successfully */
  private boolean                     projectIsBuilt;
  /** The Cabal project description */
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
    
    this.projectIsBuilt = false;
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
   * Build the Haskell project. This method creates a Job that adds a project resource scheduling
   * rule to prevent other project-referenced jobs from running, and schedules the resulting Job
   * to run.
   * 
   * @param output
   *          Echo output from the build
   * @param forceRecomp
   *          Force recompilation if true
   * @return A Job that can be scheduled to build the project
   */
  public Job buildProject(final boolean output, final boolean forceRecomp) {
    if (checkCabalFile()) {
      final String jobNamePrefix = NLS.bind(ScionText.build_job_name, getProject().getName());

      Job buildJob = new Job (jobNamePrefix) {
        @Override
        protected IStatus run(IProgressMonitor monitor) {
          try {
            monitor.beginTask(jobNamePrefix, IProgressMonitor.UNKNOWN);
            buildProjectInternal(monitor, output, forceRecomp);
            restoreState(monitor);
          } finally {
            monitor.done();
          }
          return Status.OK_STATUS;
        }
      };
      
      buildJob.setRule( project );
      buildJob.setPriority(Job.BUILD);
      buildJob.schedule();
      
      return buildJob;
    }
    
    return null;
  }
  
  /**
   * Build the Haskell project within an existing Job. Note that this is the method called when the
   * workspace is built by Eclipse.
   * 
   * @param monitor Progress and feedback
   * @param output If true, output from the build will appear in the project's console
   * @param forceRecomp If true, force recompilation
   * @return True if all commands sent to scion-server succeeded, indicating that the build completed.
   */
  public boolean buildProjectForWorkspace(final IProgressMonitor monitor, final boolean output, final boolean forceRecomp) {
    //boolean retval = false;
	final List<Boolean> retList=new LinkedList<Boolean>();
	/**
	 * there are issues between jobs and workspace operations
	 * I have seen deadlocks where a workspace operation was trying to start a job with a scheduling rule
	 * and another job with a conflicting scheduling rule was trying to start a workspace operation
	 * for example, creating/deleting modules rapidly while project is being built
	 * So here instead of using a job we use a workspace modify operation
	 */
    WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(project){
    	@Override
    	protected void execute(IProgressMonitor arg0) throws CoreException,
    			InvocationTargetException, InterruptedException {
    		 if ( checkCabalFile() ) {
    			 boolean retval = buildProjectInternal(monitor, output, forceRecomp) && restoreState(monitor);
    			 retList.add(retval);
    		 } else {
    			 retList.add(false);
    		 }
    		
    	}
    };
	try {
		wmo.run(monitor);
	} catch (InterruptedException ie){
		// noop
	}catch (InvocationTargetException ie){
		ScionPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
	}
	return retList.isEmpty()?false:retList.iterator().next();
    /*IJobManager mgr = Job.getJobManager();
   
    try {
      mgr.beginRule(project, monitor);
      if ( checkCabalFile() ) {
        retval = buildProjectInternal(monitor, output, forceRecomp) && restoreState(monitor);
      }
    } finally {
      mgr.endRule(project);
    }
    
    return retval;*/
    
  }
  
  /**
   * The internal method called by {@link #buildProject(boolean, boolean) buildProject} and 
   * {@link #buildProjectForWorkspace(IProgressMonitor, boolean, boolean) buildProjectForWorkspace} to
   * <ul>
   * <li> List the components (executables, libraries) in the Cabal project file.</li>
   * <li> Load the components into the underlying interpreter</li>
   * <li> Parse the Cabal project file and get the project description </li>
   * <li> Get the project's dependencies </li>
   * </ul>
   * 
   * @param monitor Progress feedback to the user
   * @param output If true, output from the build will appear in the project's console
   * @param forceRecomp If true, force recompilation
   * @return true if all commands sent to scion-server succeeded, indicating that the build completed.
   */
  private boolean buildProjectInternal(final IProgressMonitor monitor, final boolean output, final boolean forceRecomp) {
    boolean retval = false;
    final String projectName = project.getName();
    final String cabalFile = getCabalFile(getProject()).getLocation().toOSString();
    
    if ( listComponents(monitor) && loadComponents(monitor, output, forceRecomp) ) {
      monitor.subTask( NLS.bind( ScionText.buildProject_parseCabalDescription, projectName ) );
      ParseCabalCommand pcc = new ParseCabalCommand(cabalFile);
      if (server.sendCommand(pcc)) {
        cabalDescription = pcc.getDescription();
        packagesByDB=null;
       /* monitor.subTask( NLS.bind( ScionText.buildProject_cabalDependencies, projectName ) );
        CabalDependenciesCommand cdc = new CabalDependenciesCommand(getCabalFile(getProject()).getLocation().toOSString());
        if (server.sendCommand(cdc)) {
          packagesByDB = cdc.getPackagesByDB();
          retval = true;
        }*/
        retval = true;
      }
    }
    
    projectIsBuilt = retval;
    return retval;
  }

  /**
   * Get the list of components defined in the Cabal project file.
   * 
   * @param monitor Progress feedback
   * @return True if the the scion-server commands completed successfully.
   */
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
            if (c.isBuildable() && c.toString().equals(lastLoadedComponent.toString())) {
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
  
  /**
   * Load each component, generated by {@link #listComponents(IProgressMonitor) listComponents}.
   * 
   * @param monitor Progress feedback
   * @param output If true, capture the output of the build/load process.
   * @param forceRecomp If true, force recompilation and reloading.
   * @return
   */
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
    	if (c.isBuildable()){
	      monitor.subTask( NLS.bind ( ScionText.buildProject_loadComponents, theProject.getName(), c.toString() ) );
	
	      final LoadCommand loadCommand = new LoadCommand(theProject, c, output, forceRecomp);
	      if (server.sendCommand(loadCommand)) {
	        crh.process(loadCommand);
	        lastLoadedComponent = c;
	      } else {
	        failed = true;
	        break;
	      }
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

  /** Reset initial state. */
  private void internalReset() {
    projectIsBuilt = false;
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
  
  /**
   * Externally visible verison of reloadFile(); this is only used by PDE tests.
   * 
   * @param file The file to load
   * @return true if the scion-server commands succeeded.
   */
  public boolean loadFile ( IFile file ) {
    return reloadFile(new NullProgressMonitor(), file );
  }
  
  /**
   * Restore state after a project build. This is mainly for the convenience of workspace builds,
   * where the current file should be reloaded in scion-server.
   * 
   * @param monitor User feedback device passed to reloadFile().
   */
  private boolean restoreState( IProgressMonitor monitor ) {
    return (loadedFile == null || reloadFile( monitor, loadedFile ) );
  }

  // ////////////////////
  // External commands
  /**
   * Execute the command iff its file is currently loaded. This is synchronized to optimize
   * singleton commands operating on the same file.
   * 
   * @param f The file to test.
   * @return True if the file is the loaded file.
   */
  public boolean isLoaded(IFile f) {
    return f.equals(loadedFile);
  }

  /**
   * Setter that makes the given file the loaded file.
   * 
   * @param loadedFile The new loaded file.
   */
  public void setLoadedFile(IFile loadedFile) {
    if (!loadedFile.equals(this.loadedFile)){
	    loadedDocContents=null;
	    try {
	    	loadedDocContents=FileUtil.getContents(loadedFile);
	    } catch (Exception e){
	    	ScionPlugin.logError(ScionText.error_readingFile, e);
	    }
    }
    this.loadedFile = loadedFile;
  }

  /**
   * Delete all problem markers for a given file.
   *  
   * @param r A resource that should be a file.
   */
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
   * @return true if the component was loaded successfully or no component load was needed, otherwise
   * false indicated a scion-server interaction error.
   */
  private boolean runWithComponent( final IProgressMonitor monitor, final IFile file ) {
    boolean loadOK = true;

    if ( projectIsBuilt || buildProjectInternal(monitor, false, true) ) {
      Set<String> componentNames = resolver.getComponents( file );
      
      if (lastLoadedComponent == null || !componentNames.contains(lastLoadedComponent.toString())) {
        Component toLoad = null;
  
        if (!componentNames.isEmpty()) {
          synchronized (components) {
            for (Component compo : components) {
              if (compo.isBuildable() && componentNames.contains(compo.toString())) {
                toLoad = compo;
                break;
              }
            }
          }
        }
  
        LoadInfo li = getLoadInfo(file);
  
        // we have no component: we create a file one
        if (toLoad == null) {
          toLoad = new Component(ComponentType.FILE, file.getName(), file.getLocation().toOSString(),true);
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
          
          monitor.subTask(NLS.bind(ScionText.loadingComponent_task_name, fProject.getName(), toLoad.toString() ) );
          if ( server.sendCommand(new LoadCommand(fProject, toLoad, false, false)) ) {
            lastLoadedComponent = toLoad;
          } else {
            loadOK = false;
          }
        }
      }
    } else {
      // Build failed.
      loadOK = false;
    }

    return loadOK;
  }

  /**
   * Load a file into the scion-server, setting the proper context for
   * subsequent commands.
   * 
   * @param monitor User feedback device
   * @param file The file to be loaded
   */
  private boolean reloadFile( final IProgressMonitor monitor, final IFile file ) {
    if (runWithComponent(monitor, file)) {
      monitor.subTask( NLS.bind(ScionText.reloadFile_task_name, file.getName()));
      return server.sendCommand(new BackgroundTypecheckFileCommand( this, file ) );
    } else
      return false;
  }

  /**
   * Update (reload) the file with its current document contents, primarily used
   * to perform background type checking on the document while it's being
   * edited.
   * <p>
   * <b> THIS METHOD DOES NOT CHECK PREREQUISITES; DO NOT CALL THIS METHOD
   * DIRECTLY. USE
   * {@link #withLoadedDocument(IFile, IDocument, ScionCommand, String)} or
   * {@link #withLoadedDocumentAsync(IFile, IDocument, ScionCommand, String, IAsyncScionCommandAction)}
   * INSTEAD.</b>
   * 
   * @param file
   *          The file associated with the document
   * @param doc
   *          The document to be type-checked
   * @return true if all commands sent to the scion-server succeeded.
   */
  private boolean reloadFile(final IProgressMonitor monitor, final IFile file, final IDocument doc) {
	String newContents=doc.get();
	if(loadedDocContents==null || !newContents.equals(loadedDocContents)){
		loadedDocContents=newContents;
	    String jobName = NLS.bind(ScionText.reloadCurrentDocument_job_name, file.getName());
	    monitor.subTask(jobName);
	    final LoadInfo li = getLoadInfo(file);
	    final BackgroundTypecheckArbitraryCommand cmd = new BackgroundTypecheckArbitraryCommand(this, file, doc) {
	      @Override
	      public boolean onError(String name, String message) {
	        if (message != null && message.contains(GhcMessages.ERROR_INTERACTIVE_DISABLED)) {
	        	// I'm in two minds here: is it better to see obsolete errors at the wrong place, or to not see any error and have to save to see again the errors?
	        	//deleteProblems(file);
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
	    return server.sendCommand(cmd);
	}
	return true;
    
   
  }

  /**
   * Disassociate the current file as the loaded file. This will eventually force withLoadedFile to
   * reload the next file.
   * 
   * @param file The file to unload
   */
  public void unloadFile(IFile file) {
    Assert.isTrue( loadedFile.equals(file) );
    loadedFile = null;
  }

  /**
   * Get the "thing" indicated at the editor's point. This method operates on the current document,
   * not the currently loaded file so that the user gets updated data.
   * 
   * @param doc
   *          The current document
   * @param location
   *          The editor's point
   * @param qualify
   *          If true, return the fully qualified "thing"
   * @param typed
   *          If true, return the "thing"'s type and documentation
   * @return A string with the "thing"'s information
   */
  public String thingAtPoint(final IDocument doc, Location location, boolean qualify, boolean typed) {
    // the scion command will only work fine if we have the proper file loaded
    final IFile file = location.getIFile(getProject());
    if (file != null) {
      final String jobName = NLS.bind(ScionText.thingatpoint_job_name, file.getName());
      final ThingAtPointCommand cmd = new ThingAtPointCommand(location, qualify, typed);
      
      if (withLoadedDocument( file, doc, cmd, jobName ))
        return cmd.getThing();
    }
    
    return null;
  }

  /**
   * Get the outline for the file.
   * 
   * This method does not get the outline for the current document. Use {@link #outline(IFile, IDocument, OutlineHandler)}
   * instead.
   * 
   * @param file The file whose outline is generated
   * @return A list of outline definitions
   */
  public List<OutlineDef> outline(final IFile file) {
    final OutlineCommand cmd = new OutlineCommand(file);
    
    withLoadedFile(file, cmd, NLS.bind(ScionText.outline_job_name, file.getName()));
    return cmd.getOutlineDefs();
  }
  
  /**
   * Get the outline for the file's current document. This method operates asynchronously,
   * calling a handler's {@link OutlineHandler#handleOutline(List) handleOutline} method
   * when the generated outline becomes available. 
   * 
   * @param file The file, used to ensure that scion-server is operating on the correct file
   * @param doc The document, i.e., the file's current editor contents.
   * @param handler The OutlineHandler object who's handleOutline() method is invoked.
   */
  public void outline(final IFile file, final IDocument doc, final OutlineHandler handler) {
    if (file != null && handler != null) {
      withLoadedDocumentAsync(file, doc, new OutlineCommand(file), NLS.bind(ScionText.outline_job_name, file.getName()),
                              handler);
    }
  }
  
  /**
   * Get the Haskell lexer token preceding the editor's point
   * 
   * @param file The file being operated on
   * @param doc The current document
   * @param point The editor's point
   */
  public String tokenPrecedingPoint(final IFile file, final IDocument doc, final IRegion point) {
    try {
      Location location = new Location(file.toString(), doc, point);
      final String jobName = NLS.bind(ScionText.tokenpreceding_job_name, file.getName());
      final TokenPrecedingPoint cmd = new TokenPrecedingPoint(doc.get(), location, false);
        
      if (withLoadedDocument( file, doc, cmd, jobName ))
        return cmd.getTokenString();
    } catch (BadLocationException e) {
      // Fall through
    }
    
    return new String();
  }

  
  /**
   * Get the Haskell lexer token preceding the editor's point
   * 
   * @param file The file being operated on
   * @param doc The current document
   * @param point The editor's point
   */
  public String tokenAtPoint(final IFile file, final IDocument doc, final IRegion point) {
    try {
      Location location = new Location(file.toString(), doc, point);
      final String jobName = NLS.bind(ScionText.tokenpreceding_job_name, file.getName());
      final TokenAtPoint cmd = new TokenAtPoint(doc.get(), location, false);
        
      if (withLoadedDocument( file, doc, cmd, jobName ))
        return cmd.getTokenString();
    } catch (BadLocationException e) {
      // Fall through
    }
    
    return new String();
  }

  /**
   * Locate the first definition of a name.
   * 
   * @param name The name to located.
   * @return The editor location where the name can be found.
   */
  public Location firstDefinitionLocation(String name) {
    NameDefinitionsCommand command = new NameDefinitionsCommand(name);
    server.sendCommand(command);
    return command.getFirstLocation();
  }

  /**
   * Generate the Cabal project file's name from the Eclipse project's name.
   * 
   * @param project The Eclipse project
   * @return The "&lt;project&gt;.cabal" string.
   */
  public static IFile getCabalFile(final IProject project) {
    return project.getFile(new Path(project.getName()).addFileExtension(FileUtil.EXTENSION_CABAL));
  }

  public JSONObject getCabalDescription() {
    // Never called... :-)
    return cabalDescription;
  }

  /**
   * Accessor for the Cabal dependencies map.
   * 
   * @return The cabal dependencies map.
   */
  public Map<String, CabalPackage[]> getPackagesByDB() {
	  if (packagesByDB==null){
		  CabalDependenciesCommand cdc = new CabalDependenciesCommand(getCabalFile(getProject()).getLocation().toOSString());
	      if (withProject(cdc,  NLS.bind( ScionText.buildProject_cabalDependencies, getProject().getName() ))){
		      packagesByDB = cdc.getPackagesByDB();
	      }
	  }
    return packagesByDB;
  }

  /**
   * Accessor for the Cabal project file's components.
   * 
   * @return A list of the cabal project's components.
   */
  public List<Component> getComponents() {
    if (components == null) {
      // Attempt to build the project, since this is an indicator that the build needs to be done.
      Job buildJob = buildProject(false, true);
      
      while (buildJob.getResult() == null) {
        try {
          buildJob.join();
        } catch (InterruptedException irq) {
          break;
        }
      }
    }

    return components;
  }

  /**
   * Get the currently visible (defined) names, used for generating completions.
   * 
   * @return A list of defined names.
   */
  public List<String> definedNames( ) {
    final DefinedNamesCommand cmd = new DefinedNamesCommand();
    
    withProject( cmd, NLS.bind(ScionText.definedNames_task_name, project.getName()));
    return cmd.getNames();
  }

  /**
   * Ask the scion-server for the current project component's exposed modules. This is used
   * by the 'import' completion processing as well as the module template variable processor.
   * 
   * @return The list of Haskell modules visible to the current Haskell source file.
   */
  public List<String> listExposedModules( ) {
    if (exposedModulesCache == null) {
      final ListExposedModulesCommand cmd = new ListExposedModulesCommand();

      withProject( cmd, NLS.bind(ScionText.listExposedModules_task_name, project.getName()) );
      exposedModulesCache = Collections.unmodifiableList(cmd.getNames());
    }
    
    return exposedModulesCache;
  }

  /**
   * Ask the scion-server for the current project component's module graph. This is used by
   * the 'import' completion processing as well as the module template variable processor.
   * 
   * @return The list of Haskell modules visible in the module graph
   */
  public List<String> moduleGraph( ) {
    final ModuleGraphCommand cmd = new ModuleGraphCommand();

    withProject( cmd, NLS.bind(ScionText.moduleGraph_task_name, project.getName()) );
    return cmd.getNames();
  }

  /**
   * Fetches the syntax highlighting token type list for the current editor's Haskell source,
   * sending the file's contents.
   * 
   * @param file The current Haskell source file
   * @param contents The source file's current contents
   * @return A list of {@link TokenDef TokenDef} tokens used for syntax highlighting.
   */
  public synchronized List<TokenDef> tokenTypes(final IFile file, final String contents) {
    TokenTypesCommand command = new TokenTypesCommand(file, contents, FileUtil.hasLiterateExtension(file));
    server.sendCommand(command);
    return command.getTokens();
  }
  
  public Map<String, String> completionsForTypeConstructors(final IFile file, final IDocument doc) {
    final CompletionTyCons tycons = new CompletionTyCons(file);
    
    if (withLoadedDocument(file, doc, tycons, "TyCon completions")) {
      return tycons.getCompletions();
    }
    
    return null;
  }

  /**
   * Set the scion-server's debug level to deafening (lots of output!)
   */
  public void setDeafening() {
    server.sendCommand(new SetVerbosityCommand(3));
  }
  
  /**
   * Predicate test for whether command groups need a project rule in addition to the file rule.
   * 
   * @param file The file, used to query component names.
   * @return true if a project rule is needed.
   */
  private boolean needsProjectRule( final IFile file ) {
    Set<String> componentNames = resolver.getComponents( file );
    boolean needsComponent = (lastLoadedComponent == null || !componentNames.contains(lastLoadedComponent.toString()) );

    return !projectIsBuilt || needsComponent;
  }

  /**
   * Ensure that the command sequence is executed using a project scheduling rule. The resulting Job
   * is executed synchronously.
   */
  private boolean withProject( final ScionCommand command, final String jobName ) {
	// we want to run synchronously, and we're in the ui thread: let's not use a job!
	if (Display.getCurrent()!=null){
		
        IStatus retval = Status.CANCEL_STATUS;
        if ( projectIsBuilt || buildProjectInternal(new NullProgressMonitor(), false, true) ) {
          retval = (server.sendCommand(command) ? Status.OK_STATUS : Status.CANCEL_STATUS);
        }
        
        return retval.equals(Status.OK_STATUS);
		      
		    
	} else {
		/**
		 * there are issues between jobs and workspace operations
		 * I have seen deadlocks where a workspace operation was trying to start a job with a scheduling rule
		 * and another job with a conflicting scheduling rule was trying to start a workspace operation
		 * for example, creating/deleting modules rapidly while project is being built
		 * So here instead of using a job we use a workspace modify operation
		 */
		final List<Boolean> retList=new LinkedList<Boolean>();
	    WorkspaceModifyOperation wmo=new WorkspaceModifyOperation(project){
	    	@Override
	    	protected void execute(IProgressMonitor monitor) throws CoreException,
	    			InvocationTargetException, InterruptedException {
	    		if ( projectIsBuilt || buildProjectInternal(monitor, false, true) ) {
	    			boolean retval = server.sendCommand(command) ;
	    			retList.add(retval);
	  	        } 
	    		
	    	}
	    };
		try {
			wmo.run(new NullProgressMonitor());
		} catch (InterruptedException ie){
			// noop
		}catch (InvocationTargetException ie){
			ScionPlugin.logError(ie.getLocalizedMessage(), ie.getCause());
		}
		return retList.isEmpty()?false:retList.iterator().next();
		
	    /*Job projJob = new Job ( jobName ) {
	      @Override
	      protected IStatus run(IProgressMonitor monitor) {
	        IStatus retval = Status.CANCEL_STATUS;
	        if ( projectIsBuilt || buildProjectInternal(monitor, false, true) ) {
	          retval = (server.sendCommand(command) ? Status.OK_STATUS : Status.CANCEL_STATUS);
	        }
	        
	        return retval;
	      }
	    };
	    // Note: All of this code could go into a separate class, but this is only used
	    // once, so really no point.
	    projJob.setPriority( Job.INTERACTIVE );
	    projJob.setRule( project );
	    projJob.schedule();
	    
	    while (projJob.getResult() == null) {
	     try {
	        projJob.join();
	      } catch (InterruptedException irq) {
	        // Return something reasonable.
	        return false;
	      }
	      //Thread.yield();
	    }
	    */
		
		
		
	    //return (projJob.getResult().equals(Status.OK_STATUS));
	}

  }
  
  /**
   * Ensure that the given file is the current file before executing a command.
   * If a multi-command sequence must be executed, the command sequence is
   * executed synchronously.
   * 
   * @param file
   *          The file
   * @param cmd
   *          The command
   * @return True if all commands, including the requested command, are executed
   *         successfully.
   */
  private boolean withLoadedFile(final IFile file, final ScionCommand cmd, final String jobName) {
    CommandGroup fileGroup = new CommandGroup(jobName, file, needsProjectRule(file)) {
      @Override
      protected IStatus run(final IProgressMonitor monitor) {
        IStatus retval = Status.CANCEL_STATUS;
        
        try {
          monitor.beginTask(jobName, IProgressMonitor.UNKNOWN);
          if (   ( isLoaded( file) || reloadFile ( monitor, file) )
              && server.sendCommand( cmd ) )
            retval = Status.OK_STATUS;
        } finally {
          monitor.done();
        }
        
        return retval;
      }
    };
    
    return (fileGroup.runGroupSynchronously().equals( Status.OK_STATUS ));
  }
  
  /**
   * Perform a command on the currently loaded document's contents (vs. the currently loaded file's contents)
   * asynchronously, invoking a handler when the scion command returns a result. This method will ensure that
   * the file becomes the currently loaded file, if it isn't already.
   * 
   * @param file The currently loaded file.
   * @param doc The file's current document contents
   * @param cmd The scion-server command to be executed
   * @param jobName The name to associate with the generated file command group
   * @param handler The handler object who's {@link IAsyncScionCommandAction#handle(ScionCommand) handle} method
   * is invoked if the command is processed successfully.
   */
  private void withLoadedDocumentAsync( final IFile file, final IDocument doc, final ScionCommand cmd,
                                                     final String jobName, final IAsyncScionCommandAction handler ) {
    new CommandGroup( jobName, file, needsProjectRule(file) ) {
      @Override
      protected IStatus run(IProgressMonitor monitor) {
        IStatus retval = Status.CANCEL_STATUS;
        
        try {
          monitor.beginTask(jobName, IProgressMonitor.UNKNOWN);
          
          if ( isLoaded(file) || reloadFile (monitor, file) ) {
            // reloadFile for a document can fail, but this is OK.
            reloadFile( monitor, file, doc );
            if ( server.sendCommand(cmd) ) {
              handler.handle(cmd);
              retval = Status.OK_STATUS;
            }
          }
        } finally {
          monitor.done();
        }
        
        return retval;
      }
    }.schedule();
  }
  
  /**
   * Perform a command on the currently loaded document's contents (vs. the currently loaded file's contents).
   * This method will ensure that the file becomes the currently loaded file, if it isn't already.
   * 
   * @param file The file
   * @param doc The file's current document contents
   * @param cmd The scion-command to execute
   * @param jobName The name associated with the file command group job
   * @return true if all scion-server command executed successfully.
   */
  private boolean withLoadedDocument( final IFile file, final IDocument doc, final ScionCommand cmd, final String jobName ) {
    CommandGroup cmdGroup = new CommandGroup( jobName, file, needsProjectRule(file) ) {
      @Override
      protected IStatus run(IProgressMonitor monitor) {
        IStatus retval = Status.CANCEL_STATUS;
        
        try {
          monitor.beginTask(jobName, IProgressMonitor.UNKNOWN);
          if ( isLoaded(file) || reloadFile (monitor, file) ) {
            // Reload the document in scion-server, but we really don't care if it fails.
            reloadFile( monitor, file, doc );
            if ( server.sendCommand(cmd) ) {
              retval = Status.OK_STATUS;
            }
          }
        } finally {
          monitor.done();
        }
        
        return retval;
      }
    };
    
    return (cmdGroup.runGroupSynchronously().equals(Status.OK_STATUS));
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
