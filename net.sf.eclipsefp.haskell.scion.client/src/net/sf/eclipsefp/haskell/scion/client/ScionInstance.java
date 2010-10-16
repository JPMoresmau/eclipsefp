package net.sf.eclipsefp.haskell.scion.client;

import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ArbitraryCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckArbitraryCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckFileCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.CabalDependenciesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.CompilationResultHandler;
import net.sf.eclipsefp.haskell.scion.internal.commands.DefinedNamesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListCabalComponentsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListExposedModulesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.LoadCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ModuleGraphCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.OutlineCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ParseCabalCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.QuitCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.SetVerbosityCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.TokenTypesCommand;
import net.sf.eclipsefp.haskell.scion.internal.servers.GenericScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.GhcMessages;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;
import net.sf.eclipsefp.haskell.scion.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.util.FileUtil;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.json.JSONException;
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
public class ScionInstance implements IScionCommandRunner {
  /** The scion-server with whom this object communicates */
  private GenericScionServer                server;
  /** The project with which the scion-server is associated */
  private IProject                    project;
  private IFile                       loadedFile;

  private JSONObject                  cabalDescription;
  private Map<String, CabalPackage[]> packagesByDB;
  private List<Component>             components;
  private CabalComponentResolver      resolver;
  private Component                   lastLoadedComponent;
  private List<String>                exposedModulesCache;

  private Map<IFile, LoadInfo>        loadInfos;
  
  /** The listener list for objects interested in server status events */
  private static final ListenerList   listeners = new ListenerList();

  /** The constructor
   * 
   * @param server The scion-server instance to whom commands are sent
   * @param project The associated {@link IProject IProject}
   * @param resolver The Cabal component resolver
   */
  public ScionInstance(GenericScionServer server, IProject project, CabalComponentResolver resolver) {
    this.server = server;
    this.project = project;
    this.resolver = resolver;
    
    this.loadedFile = null;
    this.components = new LinkedList<Component>();
    this.exposedModulesCache = null;
    this.loadInfos = new HashMap<IFile, LoadInfo>();
  }

  /** Get the project associated with this instance */
  public final IProject getProject() {
    return project;
  }
  
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  // Listener management
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
  
  /** Add a scion-server event change listener */
  public static void addListener(IScionServerEventListener listener) {
    listeners.add(listener);
  }
  
  /** Remove a scion-server event change listener */
  public static void removeListener(IScionServerEventListener listener) {
    listeners.remove(listener);
  }
  
  /** Notify listeners that a scion-server event occurred.
   * 
   * @param evType The type of event that just happened.
   */
  private void notifyListeners(ScionServerEventType evType) {
    ScionServerEvent ev = new ScionServerEvent(server, evType);
    for (Object listener : listeners.getListeners()) {
      IScionServerEventListener evListener = (IScionServerEventListener) listener;
      evListener.processScionServerEvent(ev);
    }
  }

  /** Update the scion-server executable. This method is invoked in response to
   * a preference change in the UI to signal that the underlying scion-server
   * has changed.
   * 
   * @param server The scion-server executable that this instance should use.
   * @throws ScionServerStartupException if the server could not be started.
   */
  public void setServerExecutable(final GenericScionServer newServer) throws ScionServerStartupException {
    if (!server.equals(newServer)) {
      stop(true);
      server = newServer;
      start();
      notifyListeners(ScionServerEventType.EXECUTABLE_CHANGED);
    }
  }

  /** Redirect the underlying server's output stream to a new Writer. */
  public void setOutputStream(final Writer outStream) {
    server.setOutputStream(outStream);
  }
  /** Start the instance's underlying server */
  public void start() throws ScionServerStartupException {
    server.startServer(getProjectName());
    server.checkProtocol(this);
    
    if (Trace.isTracing()) {
      setDeafening();
    }
    // openCabal();
    buildProject(false, true);
    // done in buildProject
    // restoreState();
  }

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

  /*
   * public void configureCabal(IJobChangeListener listener) { if
   * (checkCabalFile()){ ConfigureCabalProjectCommand command=new
   * ConfigureCabalProjectCommand(this,Job.BUILD,getProject()); if
   * (listener!=null){ command.addJobChangeListener(listener); }
   * command.runAsync(); } }
   * 
   * public void openCabal() { if (checkCabalFile()){ OpenCabalProjectCommand
   * command = new OpenCabalProjectCommand(this,Job.BUILD,getProject());
   * command.addJobChangeListener(new JobChangeAdapter() {
   * 
   * @Override public void done(IJobChangeEvent event) { if
   * (event.getResult().isOK()) { configureCabal(null); } } });
   * command.runAsync(); }
   * 
   * }
   */

  /** Build the Haskell project.
   * 
   */
  public void buildProject(final boolean output, final boolean forceRecomp) {
    // configureCabal(new JobChangeAdapter(){
    // @Override
    // public void done(IJobChangeEvent event) {
    // if (event.getResult().isOK()) {
    this.cabalDescription = null;
    if (checkCabalFile()) {
      final ListCabalComponentsCommand command = new ListCabalComponentsCommand(getCabalFile(getProject()).getLocation().toOSString());

      command.addContinuation(new ICommandContinuation() {
        public void commandContinuation() {
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
          List<Component> cs = null;
          synchronized (components) {
            cs = new ArrayList<Component>(components);
          }
          deleteProblems(getProject());
          CompilationResultHandler crh = new CompilationResultHandler(getProject());

          for (Component c : cs) {
            LoadCommand loadCommand = new LoadCommand(getProject(), c, output, forceRecomp);
            // loadCommand.addJobChangeListener();
            server.sendCommandSync(loadCommand);
            crh.process(loadCommand);
            lastLoadedComponent = c;
          }

          ParseCabalCommand pcc = new ParseCabalCommand(getCabalFile(getProject()).getLocation().toOSString());
          server.sendCommandSync(pcc);
          ScionInstance.this.cabalDescription = pcc.getDescription();

          CabalDependenciesCommand cdc = new CabalDependenciesCommand(getCabalFile(getProject()).getLocation().toOSString());
          server.sendCommandSync(cdc);
          ScionInstance.this.packagesByDB = cdc.getPackagesByDB();

          restoreState();
        }
      });
      
      server.sendCommand(command);
    }
  }

  public void stop() {
    stop(true);
  }

  private void stop(boolean cleanly) {
    cabalDescription = null;
    synchronized (components) {
      components.clear();
    }

    packagesByDB = null;
    lastLoadedComponent = null;
    exposedModulesCache = null;
    
    if (server != null) {
      if (cleanly) {
        ScionCommand cmd = new QuitCommand();
        server.sendCommand(cmd);
      }
      // server may not be running...
      if (server != null) {
        server.stopServer();
        server = null;
      }
    }
  }

  /** Restart the scion-server
   * 
   * @param cleanly Shut down the scion-server cleanly, if true. 
   * @param inBetween Runnable action to execute between shutdown and startup.
   */
  private void restart(boolean cleanly) {
    stop(cleanly);
    try {
      start();
    } catch (Throwable t) {
      ScionPlugin.logError(ScionText.scionServerCouldNotStart_message, t);
    }
  }

  public boolean isStopped() {
    return server == null;
  }

  // //////////////////////////////
  // IScionCommandRunner methods

  public void sendCommand(ScionCommand command, IProgressMonitor monitor) throws ScionServerException, ScionCommandException {
    if (server == null) {
      throw new ScionCommandException(command, ScionText.scionServerNotRunning_message);
    }
    
    server.sendCommandSync(command);
  }

  public String getProjectName() {
    IProject project = getProject();
    if (project != null) {
      return project.getName();
    }
    return ScionText.noproject;
  }

  public boolean contains(ISchedulingRule rule) {
    return rule == this || (getProject() != null && rule == getProject())
        || (getProject() != null && (getProject().contains(rule)));
  }

  public boolean isConflicting(ISchedulingRule rule) {
    return rule == this;
  }

  // ////////////////////
  // Internal commands

  private void restoreState() {
    if (loadedFile != null) {
      // loadFile(loadedFile,false);
      final LoadInfo li = getLoadInfo(loadedFile);
      if (li.lastCommand != null) {
        li.lastCommand = null;
      }

      /*
       * Runnable run=new Runnable(){ public void run() {
       * BackgroundTypecheckFileCommand cmd = new
       * BackgroundTypecheckFileCommand(ScionInstance.this, file);
       * li.lastCommand=cmd; cmd.addJobChangeListener(l2);
       * ScionInstance.this.run(cmd,after,sync); }; };
       */
      BackgroundTypecheckFileCommand cmd = new BackgroundTypecheckFileCommand(ScionInstance.this, loadedFile);
      cmd.addContinuation(new ICommandContinuation() {
        public void commandContinuation() {
          li.lastCommand = null;
        }
      } );
      
      li.lastCommand = cmd;
      server.sendCommand(cmd);
    }
  }

  // ////////////////////
  // External commands

  /*
   * public void backgroundTypecheckFile(IFile file) {
   * BackgroundTypecheckFileCommand command = new
   * BackgroundTypecheckFileCommand(this, file); command.runAsync(); }
   */

  /*
   * public void backgroundTypecheckArbitrary(final IFile file,IDocument doc) {
   * BackgroundTypecheckArbitraryCommand cmd = new
   * BackgroundTypecheckArbitraryCommand(this, file,doc);
   * cmd.addJobChangeListener(new JobChangeAdapter(){
   * 
   * @Override public void done(IJobChangeEvent event) { if
   * (!event.getResult().isOK()){ ScionInstance.this.reloadFile(file, null); } }
   * }); cmd.runAsync(); }
   */

  public void loadFile(IFile fileName, boolean sync) {
    // loadedFiles.add(fileName);
    reloadFile(fileName, (ScionCommand) null, sync);
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

  private void runWithComponent(final IFile file, final ScionCommand after, final boolean sync) {
    ScionCommand cmd = new ArbitraryCommand() {
      @Override
      protected void doProcessResult(Object result) throws JSONException {
        Set<String> componentNames = resolver.getComponents(file);
        if (lastLoadedComponent == null || !componentNames.contains(lastLoadedComponent.toString())) {
          Component toLoad = null;

          if (!componentNames.isEmpty()) {
            synchronized (components) {
              for (final Component compo : components) {
                if (componentNames.contains(compo.toString())) {
                  toLoad = compo;
                  break;
                }
              }
            }
          }
          
          final LoadInfo li = getLoadInfo(file);
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
            final Component compo = toLoad;
            LoadCommand loadCommand = new LoadCommand(getProject(), compo, false, false);
            if (after != null) {
              loadCommand.addSuccessor(after);
            }
            
            this.addContinuation(new ICommandContinuation() {
              public void commandContinuation() {
                lastLoadedComponent = compo;
              }
            } );
            this.addSuccessor(loadCommand);
          }
        } else {
          if (after != null) {
            this.addSuccessor(after);
          }
        }
      }
    };
    
    doCommand(cmd, null, sync);
  }

  public void reloadFile(final IFile file, final ICommandContinuation after, final boolean sync) {
    final LoadInfo li = getLoadInfo(file);
    if (li.lastCommand != null) {
      li.lastCommand = null;
    }

    BackgroundTypecheckFileCommand cmd = new BackgroundTypecheckFileCommand(ScionInstance.this, file);
    li.lastCommand = cmd;
    cmd.addContinuation(new ICommandContinuation() {
      public void commandContinuation() {
        li.lastCommand = null;
      }
    });
    
    if (after != null) {
      cmd.addContinuation(after);
    }

    runWithComponent(file, cmd, sync);
  }

  public void reloadFile(final IFile file, final ScionCommand after, final boolean sync) {
    final LoadInfo li = getLoadInfo(file);
    if (li.lastCommand != null) {
      li.lastCommand = null;
    }

    /*
     * Runnable run=new Runnable(){ public void run() {
     * BackgroundTypecheckFileCommand cmd = new
     * BackgroundTypecheckFileCommand(ScionInstance.this, file);
     * li.lastCommand=cmd; cmd.addJobChangeListener(l2);
     * ScionInstance.this.run(cmd,after,sync); }; };
     */
    BackgroundTypecheckFileCommand cmd = new BackgroundTypecheckFileCommand(ScionInstance.this, file);
    cmd.addContinuation(new ICommandContinuation() {
      public void commandContinuation() {
        li.lastCommand = null;
      }
    } );
    li.lastCommand = cmd;
    if (after != null) {
      cmd.addSuccessor(after);
    }
    runWithComponent(file, cmd, sync);
  }

  public void reloadFile(final IFile file, final IDocument doc, final ICommandContinuation after, final boolean sync) {
    // done on return
    // deleteProblems(file);
    // LoadCommand loadCommand = new LoadCommand(this, new
    // Component(ComponentType.FILE,file.getLocation().toOSString(),getCabalFile(getProject()).getLocation().toOSString()),false);
    // final IJobChangeListener l=new
    // CompilationResultHandler(getProject(),doc);
    final LoadInfo li = getLoadInfo(file);
    if (li.lastCommand != null) {
      li.lastCommand = null;
    }

    BackgroundTypecheckArbitraryCommand cmd = new BackgroundTypecheckArbitraryCommand(this, file, doc) {
      @Override
      public boolean onError(JSONException ex, String name, String message) {
        li.lastCommand = null;
        if (message != null && message.contains(GhcMessages.ERROR_INTERACTIVE_DISABLED)) {
          deleteProblems(file);
          if (!li.interactiveCheckDisabled) {
            ScionPlugin.logWarning(
                ScionText.bind(ScionText.warning_typecheck_arbitrary_failed, file.getProjectRelativePath(), message), null);
            li.interactiveCheckDisabled = true;
          }
          // removeJobChangeListener(l);
          // removeJobChangeListener(l2);
          // ScionInstance.this.reloadFile(file, after,sync);

          return true;
        }
        li.interactiveCheckDisabled = false;
        return super.onError(ex, name, message);

      }
    };
    li.lastCommand = cmd;
    // cmd.addJobChangeListener(l);
    cmd.addContinuation(new ICommandContinuation() {
      public void commandContinuation() {
        li.lastCommand = null;
      }
    } );
    if (after != null) {
      cmd.addContinuation(after);
    }
    
    doCommand(cmd, null, sync);
    // loadCommand.addSuccessor(typecheckCommand);
    // loadCommand.runAsync();
    // typecheckCommand.runAsyncAfter(loadCommand);
  }

  private void doCommand(ScionCommand command, final ICommandContinuation after, boolean sync) {
    if (after != null) {
      command.addContinuation(after);
    }
    if (sync) {
      server.sendCommandSync(command);
    } else {
      server.sendCommand(command);
    }
  }

  public void unloadFile(IFile fileName) {
    if (fileName.equals(loadedFile)) {
      loadedFile = null;
    }
  }

  public String thingAtPoint(Location location) {
    ThingAtPointCommand command = new ThingAtPointCommand(location);
    if (server.sendCommandSync(command)) {
      return command.getThing();
    } else {
      return null;
    }
  }

  public void outline(final IFile file, final OutlineHandler handler, final boolean sync) {
    final OutlineCommand cmd = new OutlineCommand(file);
    if (handler != null) {
      cmd.addContinuation(new ICommandContinuation() {
        public void commandContinuation() {
          handler.outlineResult(cmd.getOutlineDefs());
        }
      } );
    }

    withLoadedFile(file, cmd, sync);
  }

  public void withLoadedFile(final IFile file, ScionCommand cmd, final boolean sync) {
    if (isLoaded(file)) {
      doCommand(cmd, null, sync);
    } else {
      reloadFile(file, cmd, sync);
    }
  }

  public Location firstDefinitionLocation(String name) {
    NameDefinitionsCommand command = new NameDefinitionsCommand(name);
    if (server.sendCommandSync(command)) {
      return command.getFirstLocation();
    } else {
      return null;
    }
  }

  public static IFile getCabalFile(final IProject p) {
    String ext = FileUtil.EXTENSION_CABAL;
    IPath path = new Path(p.getName()).addFileExtension(ext);
    return p.getFile(path);
  }

  public JSONObject getCabalDescription() {
    return cabalDescription;
  }

  public Map<String, CabalPackage[]> getPackagesByDB() {
    return packagesByDB;
  }

  public List<Component> getComponents() {
    return components;
  }

  public void definedNames(final NameHandler handler) {
    if (handler != null) {
      final DefinedNamesCommand command = new DefinedNamesCommand();
      command.addContinuation( new ICommandContinuation() {
            public void commandContinuation() {
              handler.nameResult(command.getNames());
            }
          } );
      server.sendCommandSync(command);
    }
  }

  public void listExposedModules(final NameHandler handler) {
    if (handler != null) {
      if (exposedModulesCache != null) {
        handler.nameResult(exposedModulesCache);
        return;
      }
      final ListExposedModulesCommand command = new ListExposedModulesCommand();
      command.addContinuation(new ICommandContinuation() {
        public void commandContinuation() {
          exposedModulesCache = Collections.unmodifiableList(command.getNames());
          handler.nameResult(exposedModulesCache);
        }
      } );

      server.sendCommandSync(command);
    }
  }

  public void moduleGraph(final NameHandler handler) {
    if (handler != null) {
      final ModuleGraphCommand command = new ModuleGraphCommand();
      command.addContinuation(new ICommandContinuation() {
        public void commandContinuation() {
          handler.nameResult(command.getNames());
        }
      } );
      server.sendCommandSync(command);
    }

  }

  public synchronized List<TokenDef> tokenTypes(final IFile file, final String contents) {
    TokenTypesCommand command = new TokenTypesCommand(file, contents, FileUtil.hasLiterateExtension(file));
    server.sendCommandSync(command);
    return command.getTokens();
  }

  public synchronized void setDeafening() {
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
    private boolean      interactiveCheckDisabled = false;
    private boolean      useFileComponent         = false;
    private ScionCommand lastCommand;

  }

}
