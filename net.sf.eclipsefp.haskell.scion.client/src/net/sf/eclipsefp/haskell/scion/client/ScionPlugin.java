// Copyright (c) 2009 by Thomas ten Cate <ttencate@gmail.com>
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.InputStream;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.servers.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.NetworkScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.NullScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.StdStreamScionServer;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.NullWriter;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.BundleContext;

public class ScionPlugin extends AbstractUIPlugin {
  /**
   * The plugin's bundle name (used for identifying resources, giving the plugin
   * a unique identifier
   */
  private static final String                BUNDLE_NAME               = ScionPlugin.class.getCanonicalName();
  /** The default instance */
  private static ScionPlugin                 pluginInstance;
  /** Project problem marker identifier */
  public static final String                 ID_PROJECT_PROBLEM_MARKER = BUNDLE_NAME + ".projectProblem";       //$NON-NLS-1$

  /**
   * The name of the sub-directory in the state location area where the built-in
   * server lives
   */
  public static final String                 DIST_FOLDER               = "dist-scion";
  /** Version of the scion zip file containing the built-in server's source */
  public static final String                 SCION_VERSION             = "0.1.0.5";
  /** The scion server factory */
  private ScionServerFactory                 serverFactory;
  /** The project -> scion instance map */
  private final Map<IProject, InstanceState> instances;
  /** The shared instance, primarily used for lexical analysis
   * 
   * @note This is a separate object to prevent looking for the null project in the {@link #instances} map.
   */
  private final InstanceState                sharedScionInstance;
  /** The version number of the Scion protocol that we support. */
  public static final int                    PROTOCOL_VERSION          = 1;
  /** The plugin's resource bundle. */
  private ResourceBundle                     resourceBundle;
  
  /** Instance state associated with each project */
  private class InstanceState {
    /** The scion server instance */
    private ScionInstance instance;
    /** The output stream for the server */
    private Writer outStream;
    /** The constructor */
    public InstanceState(ScionInstance instance, Writer outStream) {
      this.instance = instance;
      this.outStream = outStream;
    }
    /** Get the instance */
    public ScionInstance getInstance() {
      return instance;
    }
    /** Get the output stream */
    public Writer getOutStream() {
      return outStream;
    }
  }
  /** Container class for the {@link NullScionServerFactory NullScionServerFactory} singleton
   * instance.
   */
  private final static class NullServerSingletonContainer {
    private final static NullScionServerFactory theInstance = getDefault().new NullScionServerFactory();
  }

  /**
   * The default constructor.
   * 
   * @note: There has to be a better way to support plugin singletons and
   *        instance assignment.
   */
  public ScionPlugin() {
    pluginInstance = this;
    
    // Set reasonable defaults that can be updated later:
    serverFactory = getNullServerFactory();
    instances = new HashMap<IProject, InstanceState>();
    
    Writer outStream = new NullWriter();
    ScionServer server = serverFactory.createScionServer(null, outStream);
    ScionInstance scionInstance = new ScionInstance(server, null, null);
    sharedScionInstance = new InstanceState(scionInstance, outStream);
  }

  @Override
  public void start(BundleContext context) throws Exception {
    super.start(context);
    resourceBundle = ResourceBundle.getBundle("plugin");
  }

  @Override
  public void stop(BundleContext context) throws Exception {
    super.stop(context);
  }

  public static String getPluginId() {
    if (pluginInstance != null) {
      return pluginInstance.getBundle().getSymbolicName();
    } else {
      return BUNDLE_NAME; // fallback, but bad for mantainability...
    }
  }

  /** Get the default Scion plug-in instance */
  public static ScionPlugin getDefault() {
    return pluginInstance;
  }
  
  /** Get the singleton factory instance */
  public final static NullScionServerFactory getNullServerFactory() {
    return NullServerSingletonContainer.theInstance;
  }

  public static String getStringResource(String key) {
    ScionPlugin p = getDefault();
    if (p != null) {
      try {
        return p.resourceBundle.getString(key);
      } catch (MissingResourceException ex) {
        return key;
      }
    } else {
      // plugin still loading
      return key;
    }
  }

  /**
   * Returns whether tracing is enabled for the specified option.
   */
  public static boolean isTracing(String optionId) {
    String option = getPluginId() + "/" + optionId;
    String value = Platform.getDebugOption(option);
    return value != null && value.equalsIgnoreCase("true");
  }

  public static void logInfo(String message) {
    log(Status.INFO, message, null);
  }

  public static void logWarning(String message, Throwable cause) {
    log(Status.WARNING, message, cause);
  }

  public static void logWarning(ScionCommand command, String message, Throwable cause) {
    log(Status.WARNING, message + "\n" + command.getErrorInfo(), cause);
  }

  public static void logError(String message, Throwable cause) {
    log(Status.ERROR, message, cause);
  }

  public static void log(int severity, String message, Throwable cause) {
    Status status = new Status(severity, ScionPlugin.getPluginId(), severity, message, cause);
    logStatus(status);
  }

  public static void logStatus(IStatus status) {
    StatusManager.getManager().handle(status);
  }

  /** Get the {@link ScionInstance ScionInstance} associated with a resource's project from
   * a static context.
   * 
   * @param resource The resource whose project is associated with a {@link ScionInstance ScionInstance}.
   * @return The associated {@link ScionInstance ScionInstance} or null, if no association exists.
   */
  public static ScionInstance getScionInstance( final IResource resource ) {
    IProject project = resource.getProject();
    if (project != null) {
      return getScionInstance(project);
    }
    return null;
  }
  /** Get the {@link ScionInstance ScionInstance} associated with a project from a static
   * context.
   * 
   * @param project The project whose {@link ScionInstance ScionInstance} is requested.
   * @return The associated {@link ScionInstance ScionInstance} or null, if no association exists.
   */
  public static ScionInstance getScionInstance( final IProject project ) {
    InstanceState instState = getDefault().instances.get(project);
    return (instState != null ? instState.getInstance() : null);
  }
  /** Get the shared {@link ScionInstance ScionInstance} in a static context.
   * 
   * @return The shared {@link ScionInstance ScionInstance} instance.
   */
  public static ScionInstance getSharedScionInstance() {
    return getDefault().sharedScionInstance.getInstance();
  }
  /** Use the null scion server factory. */
  public synchronized static void useNullScionServerFactory() throws ScionServerStartupException {
    getDefault().changeServerFactory(getNullServerFactory());
  }
  
  /** Use the built-in standard stream scion server factory */
  public synchronized static void useBuiltInStdStreamServerFactory() throws ScionServerStartupException {
    getDefault().changeServerFactory(getDefault().new BuiltInStdStreamServerFactory());
  }
  
  /** Use the built-in network pipe scion server factory */
  public synchronized static void useBuiltInNetworkServerFactory() throws ScionServerStartupException {
    getDefault().changeServerFactory(getDefault().new BuiltInNetworkServerFactory());
  }
  
  /** Use the standard I/O scion server factory */
  public synchronized static void useStdStreamScionServerFactory(final IPath userExecutable) throws ScionServerStartupException {
    getDefault().changeServerFactory(getDefault().new StdStreamScionServerFactory(userExecutable));
  }
  
  /** Use the network pipe scion server factory */
  public synchronized static void useNetworkStreamScionServerFactory(final IPath userExecutable)
    throws ScionServerStartupException {
    getDefault().changeServerFactory(getDefault().new NetworkStreamScionServerFactory(userExecutable));
  }
  /** Get the current factory's executable path */
  public static IPath getFactoryExecutablePath() {
    return getDefault().serverFactory.getServerExecutable();
  }
  /** Stop all scion-servers and reset the internal project -> scion instance associations */
  public static void stopAllInstances() {
    ScionPlugin thePlugin = getDefault();
    
    for( IProject project: thePlugin.instances.keySet() ) {
      InstanceState instanceState = thePlugin.instances.get( project );
      ScionInstance scionInstance = instanceState.getInstance();
      scionInstance.stop();
    }
    thePlugin.instances.clear();
  }
  /** Create a new {@link ScionInstance ScionInstance}, using a scion-server instance from the current factory */
  public synchronized static ScionInstance createScionInstance(IProject project, Writer outStream, CabalComponentResolver resolver) {
    return getDefault().newScionInstance(project, outStream, resolver);
  }
  /** Terminate a ScionInstance, remove its association from the project -> instance association map.
   * 
   * @param Resource whose project is used to look up the associated scion instance.
   * @return True, if the resource's project exists in the {@link instances instances} map and the instance was
   *         stopped, otherwise false.
   */
  public synchronized static boolean terminateScionInstance(final IResource resource) {
    IProject project = resource.getProject();
    if (project != null) {
      return terminateScionInstance(project);
    }
    return false;
  }
  /** Terminate a ScionInstance, remove its association from the project -> instance association map.
   * 
   * @param Resource whose project is used to look up the associated scion instance.
   * @return True, if the resource's project exists in the {@link instances instances} map and the instance was
   *         stopped, otherwise false.
   */
  public static boolean terminateScionInstance(final IProject project) {
    InstanceState instState = getDefault().instances.remove(project);
    if (instState != null) {
      ScionInstance scionInstance = instState.getInstance();
      scionInstance.stop();
      return true;
    }
    return false;
  }

  /** Set the output stream writer for the shared ScionInstance */
  public synchronized static void setSharedInstanceWriter(final Writer outStream) {
    ScionPlugin plugin = getDefault();
    
    plugin.sharedScionInstance.instance.setOutputStream(outStream);
    plugin.sharedScionInstance.outStream = outStream;
  }
  /** Create a new ScionInstance, using a scion-server instance from the current factory */
  private synchronized ScionInstance newScionInstance(IProject project, Writer outStream, CabalComponentResolver resolver) {
    ScionInstance scionInstance = new ScionInstance(createScionServer(project, outStream), project, resolver); 
    instances.put(project, new InstanceState(scionInstance, outStream));
    return scionInstance;
  }
  /** Create a new ScionServer from the factory, optionally registering the server in the
   * instances map.
   */
  public synchronized ScionServer createScionServer(IProject project, Writer outStream) {
    return serverFactory.createScionServer(project, outStream);
  }
  /** Change the instances' concept of the current executable */
  private void changeServerFactory(ScionServerFactory factory) throws ScionServerStartupException {
    boolean yelp = false;
    ScionServerStartupException startupEx = null;
    
    // Set the factory
    serverFactory = factory;
    // Updated the shared instance:
    ScionInstance sharedState = sharedScionInstance.getInstance();
    Writer sharedOutStream = sharedScionInstance.getOutStream();
    try {
      sharedState.setServerExecutable(serverFactory.createScionServer(null, sharedOutStream));
    } catch (ScionServerStartupException ex) {
      startupEx = ex;
      yelp = true;
    }
    
    if (!yelp) {
      // Update the instances with the new scion servers
      for (Map.Entry<IProject, InstanceState> pair : instances.entrySet()) {
        IProject project = pair.getKey();
        InstanceState instState = pair.getValue();
        Writer outStream = instState.getOutStream();
        
        try {
          instState.getInstance().setServerExecutable(serverFactory.createScionServer(project, outStream));
        } catch (ScionServerStartupException ex) {
          if (!yelp && startupEx == null) {
            yelp = true;
            startupEx = ex;
          }
        }
      }
    }
    
    // Encountered a startup error, only yelp at the user once.
    if (yelp) {
      try {
        // Revert back to the NullScionServerFactory
        changeServerFactory(getNullServerFactory());
      } catch (ScionServerStartupException ex) {
        // Ignore it, since it cannot happen. Completeness.
      }
      // only do this once
      String errMsg = new String();
      
      errMsg = errMsg.concat(">>> Could not start server, path is").concat(PlatformUtil.NL);
      errMsg = errMsg.concat(serverFactory.getServerExecutable().toOSString()).concat(PlatformUtil.NL);
      logError(errMsg, startupEx);
      throw startupEx;
    }
  }

  /**
   * Generate the built-in Scion server's build area directory path.
   * 
   * @return An IPath to the build area subdirectory off the workspace's state
   *         location.
   */
  public static IPath builtinServerDirectoryPath() {
    return getDefault().getStateLocation().append("scion-".concat(SCION_VERSION)); //$NON-NLS-1$
  }

  /**
   * Open the Scion server's zip archive as an input stream.
   * 
   * @return The InputStream.
   */
  public static InputStream builinServerArchive() {
    String zipArchive = "scion-" + SCION_VERSION + ".zip"; //$NON-NLS-1$ //$NON-NLS-2$
    return ScionPlugin.class.getResourceAsStream(zipArchive);
  }

  /**
   * Generate the built-in server's executable path, where we'd expect to find
   * it relative to the build directory. Note that destDir is most likely the
   * same as what {@link #builtinServerDirectoryPath builtinServerDirectoryPath}
   * generated.
   * 
   * @param destDir
   *          The destination directory where the built-in scion server is being
   *          built.
   */
  public static IPath serverExecutablePath(IPath destDir) {
    IPath exePath = destDir.append("dist").append("build").append("scion-server"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    return exePath.append(FileUtil.makeExecutableName("scion-server"));
  }

  /**
   * Generate the built-in server's executable path in the default state
   * location's directory.
   * 
   * @return An IPath to the built-in server's executable.
   */
  public static IPath builtinServerExecutablePath() {
    return serverExecutablePath(builtinServerDirectoryPath());
  }
  
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
  // Internal factory classes:
  //-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
  /** Scion executable factory interface.
   * 
   * @author B. Scott Michel (scooter.phd@gmail.com)
   */
  public interface ScionServerFactory {
    /** Create a new ScionExectable 
     * @param project The associated project, which identifies the working directory for the server
     * @param outStream The output stream where the server's stdout and stderr are directed.
     */
    public ScionServer createScionServer(final IProject project, final Writer outStream);
    /** Get the server executable IPath */
    public IPath getServerExecutable();
  }
  
  public class NullScionServerFactory implements ScionServerFactory {
    
    /** Default constructor. This is hidden so preserve singleton semantics. */
    private NullScionServerFactory() {
      // NOP
    }

    /** Create a new NullScionServer. In reality, this just returns another reference
     * to the {@link NullScionServer NullScionServer}'s singleton. */
    public ScionServer createScionServer(final IProject project, final Writer outStream) {
      return NullScionServer.getDefault();
    }

    public IPath getServerExecutable() {
      return new Path("/nullServer");
    }
  }
  
  public class BuiltInStdStreamServerFactory implements ScionServerFactory {
    /** Default constructor. */
    public BuiltInStdStreamServerFactory() {
      // NOP
    }

    /** Generate a new BuiltInServer instance */
    public ScionServer createScionServer(IProject project, Writer outStream) {
      File directory = (project != null) ? new File(project.getLocation().toOSString()) : null;
      return new StdStreamScionServer(project, ScionPlugin.builtinServerExecutablePath(), outStream, directory);
    }

    public IPath getServerExecutable() {
      return ScionPlugin.builtinServerExecutablePath();
    }
  }

  public class BuiltInNetworkServerFactory implements ScionServerFactory {
    /** Default constructor. */
    public BuiltInNetworkServerFactory() {
      // NOP
    }

    /** Generate a new BuiltInServer instance */
    public ScionServer createScionServer(IProject project, Writer outStream) {
      File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
      return new NetworkScionServer(project, ScionPlugin.builtinServerExecutablePath(), outStream, directory);
    }

    public IPath getServerExecutable() {
      return ScionPlugin.builtinServerExecutablePath();
    }
  }

  public class NetworkStreamScionServerFactory implements ScionServerFactory {
    /** The current path to the user's scion-server executable */
    private IPath userExecutable;
    
    public NetworkStreamScionServerFactory(final IPath userExecutable) {
      this.userExecutable = userExecutable;
    }
    
    public ScionServer createScionServer(IProject project, Writer outStream) {
      File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
      return new NetworkScionServer(project, userExecutable, outStream, directory);
    }

    public IPath getServerExecutable() {
      return userExecutable;
    }
  }
  
  public class StdStreamScionServerFactory implements ScionServerFactory {
    /** The path to the user's executable */
    private IPath userExecutablePath;
    
    /* Constructor
     * 
     * @param userExecutablePath An IPath to the user's executable.
     */
    public StdStreamScionServerFactory(final IPath userExecutablePath) {
      this.userExecutablePath = userExecutablePath;
    }
    
    /** Create a new user-specified scion server */
    public ScionServer createScionServer(final IProject project, final Writer outStream) {
      File directory = (project !=null) ? new File(project.getLocation().toOSString()) : null;
      return new StdStreamScionServer(null, userExecutablePath, outStream, directory); 
    }

    public IPath getServerExecutable() {
      return userExecutablePath;
    }
  }
}
