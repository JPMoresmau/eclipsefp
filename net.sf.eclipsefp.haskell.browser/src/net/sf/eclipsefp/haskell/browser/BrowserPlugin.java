/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

import java.io.InputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import net.sf.eclipsefp.haskell.browser.client.NullBrowserServer;
import net.sf.eclipsefp.haskell.browser.client.StreamBrowserServer;
import net.sf.eclipsefp.haskell.util.FileUtil;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class BrowserPlugin extends AbstractUIPlugin implements IDatabaseLoadedListener, IHoogleLoadedListener {
	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.browser"; //$NON-NLS-1$
	@Deprecated
	public static final String BROWSER_VERSION = "0.1.1";
	public static final String DIST_FOLDER = ".dist-scion-browser";
	
	/** The plugin's resource bundle. */
	private ResourceBundle resourceBundle;
	
	// The shared instance
	private static BrowserPlugin plugin;

	// The instance of the server used for communication
	private BrowserServer server;
	// The console log output
	private Writer logStream;
	
	// Listeners for loading new databases
	private ArrayList<IDatabaseLoadedListener> dbLoadedListeners;
	private ArrayList<IHoogleLoadedListener> hoogleLoadedListeners;

	/**
	 * The constructor
	 */
	public BrowserPlugin() {
		this.server = new NullBrowserServer();
		this.logStream = null;
		this.dbLoadedListeners = new ArrayList<IDatabaseLoadedListener>();
		this.hoogleLoadedListeners = new ArrayList<IHoogleLoadedListener>();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
	 * )
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	  /**
	   * Get the plugin's resource bundle (aka "plugin.properties").
	   */
	  public ResourceBundle getResourceBundle() {
	    if (resourceBundle == null) {
	      try {
	        resourceBundle = Platform.getResourceBundle( getBundle() );
	      } catch( MissingResourceException x ) {
	        resourceBundle = null;
	      }
	    }

	    return resourceBundle;
	  }
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
	 * )
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		if (server != null)
			server.stop();
		super.stop(context);
	}
	
	public static String getStringResource(String key) {
		BrowserPlugin p = getDefault();
		if (p != null) {
			try {
				return p.getResourceBundle().getString(key);
			} catch (MissingResourceException ex) {
				return key;
			}
		} else {
			// plugin still loading
			return key;
		}
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static BrowserPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns an image descriptor for the image file at the given plug-in
	 * relative path
	 * 
	 * @param path
	 *            the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}

	public BrowserServer getInstance() {
		return this.server;
	}

	/**
	 * Return the shared instance of the browser server
	 * That instance should be the only one in the workspace
	 * 
	 * @return the shared instance
	 */
	public static BrowserServer getSharedInstance() {
		return getDefault().server;
	}

	/**
	 * Sets the console where scion-browser output will be redirected.
	 * 
	 * @param logStream Writer which will receive the info
	 */
	public void setLogStream(Writer logStream) {
		this.logStream = logStream;
		this.server.setLogStream(logStream);
	}

	/**
	 * Static version of {@link BrowserPlugin#setLogStream(Writer)}
	 * 
	 * @param logStream
	 */
	public static void setSharedLogStream(Writer logStream) {
		getDefault().setLogStream(logStream);
	}

	/**
	 * Changes the scion-browser used to get information about packages
	 * If the browser cannot be loaded, an empty browser will be used
	 * 
	 * @param path file path to the server executable
	 */
	public void changeInstance(IPath path) {
		// Destroy previous scion-browser
		if (this.server != null)
			this.server.stop();
		
		if (path.toFile().exists()) {
			try {
				this.server = new StreamBrowserServer(path);
				this.server.addDatabaseLoadedListener(this);
				this.server.addHoogleLoadedListener(this);
				this.server.setLogStream(this.logStream);
			} catch (Throwable ex) {
				this.server = new NullBrowserServer();
			}
		} else {
			this.server = new NullBrowserServer();
		}
	}

	/**
	 * Static version of {@link BrowserPlugin#changeInstance(IPath)}
	 * 
	 * @param path
	 */
	public static void changeSharedInstance(IPath path) {
		getDefault().changeInstance(path);
	}

	/**
	 * Makes the plug-in to use a null browser, that is, a browser
	 * which will return empty sets for any query
	 */
	public void useNullInstance() {
		// Destroy previous scion-browser
		if (this.server != null)
			this.server.stop();
		
		this.server = new NullBrowserServer();
	}

	/**
	 * Static version of {@link BrowserPlugin#useNullInstance()}
	 */
	public static void useNullSharedInstance() {
		getDefault().useNullInstance();
	}
	
	/**
	 * Makes the plug-in to use the buil-in browser
	 */
	public static void useSharedBuiltinInstance() {
		IPath path = builtinServerExecutablePath();
		changeSharedInstance(path);
	}

	/**
	 * Loads the local database in the current shared instance
	 * 
	 * @param rebuild if true, new packages installed are added to the database
	 * @return OK or ERROR
	 */
	public static IStatus loadLocalDatabase(boolean rebuild) {
		try {
			getSharedInstance().loadLocalDatabase(getLocalDatabasePath().toOSString(), rebuild);
			return Status.OK_STATUS;
		} catch (Throwable ex) {
			return new Status(Status.ERROR, PLUGIN_ID, "", ex);
		}
	}

	/**
	 * Returns the path to the place where databases are saved
	 * If it does not exists, the directory is created
	 * 
	 * @return the path to the directory
	 */
	@Deprecated
	public static IPath builtinBrowserDirectoryPath() {
		IPath path = getDefault().getStateLocation().append(
				"scion-browser-".concat(BROWSER_VERSION).concat("-dbs")); //$NON-NLS-1$
		if (!path.toFile().exists())
			path.toFile().mkdirs();
		return path;
	}

	/**
	 * Returns the path to the local database
	 * 
	 * @return the path
	 */
	public static IPath getLocalDatabasePath() {
		return builtinBrowserDirectoryPath().append("local.db");
	}

	/**
	 * Returns the path to the Hackage database
	 * 
	 * @return the path
	 */
	public static IPath getHackageDatabasePath() {
		return builtinBrowserDirectoryPath().append("hackage.db");
	}
	
	/**
	 * Adds a new listener for database changes
	 * 
	 * @param listener
	 */
	public void addDatabaseLoadedListener(IDatabaseLoadedListener listener) {
		dbLoadedListeners.add(listener);
	}
	
	/**
	 * Adds a new listener for Hoogle changes
	 * 
	 * @param listener
	 */
	public void addHoogleLoadedListener(IHoogleLoadedListener listener) {
		hoogleLoadedListeners.add(listener);
	}
	
	/**
	 * Raises an event for all listeners currently registered
	 * 
	 * @param e
	 */
	protected void notifyDatabaseLoaded(DatabaseLoadedEvent e) {
		for (IDatabaseLoadedListener listener : dbLoadedListeners)
			listener.databaseLoaded(e);
	}
	
	/**
	 * Raises an event for all listeners currently registered
	 * 
	 * @param e
	 */
	protected void notifyDatabaseUnloaded(BrowserEvent e) {
		for (IDatabaseLoadedListener listener : dbLoadedListeners)
			listener.databaseUnloaded(e);
	}
	
	/**
	 * Raises an event for all listeners currently registered
	 * 
	 * @param e
	 */
	protected void notifyHoogleLoaded(BrowserEvent e) {
		for (IHoogleLoadedListener listener : hoogleLoadedListeners)
			listener.hoogleLoaded(e);
	}
	
	/**
	 * Raises an event for all listeners currently registered
	 * 
	 * @param e
	 */
	protected void notifyHoogleUnloaded(BrowserEvent e) {
		for (IHoogleLoadedListener listener : hoogleLoadedListeners)
			listener.hoogleUnloaded(e);
	}

	public void databaseLoaded(DatabaseLoadedEvent e) {
		notifyDatabaseLoaded(e);
	}
	
	public void databaseUnloaded(BrowserEvent e) {
		notifyDatabaseUnloaded(e);
	}
	
	public void hoogleLoaded(BrowserEvent e) {
		notifyHoogleLoaded(e);
	}
	
	public void hoogleUnloaded(BrowserEvent e) {
		notifyHoogleUnloaded(e);
	}
	
	public boolean isDatabaseLoaded() {
		if (this.server == null)
			return false;
		return this.server.isDatabaseLoaded();
	}
	
	public boolean isHoogleLoaded() {
		if (this.server == null)
			return false;
		return this.server.isHoogleLoaded();
	}
	
	/**
	 * Generate the built-in Scion server's build area directory path.
	 * 
	 * @return An IPath to the build area subdirectory off the workspace's state
	 *         location.
	 */
	@Deprecated
	public static IPath builtinServerDirectoryPath() {
		return getDefault().getStateLocation().append("scion-browser-".concat(BROWSER_VERSION));
	}

	/**
	 * Open the Scion server's zip archive as an input stream.
	 * 
	 * @return The InputStream.
	 */
	@Deprecated
	public static InputStream builinServerArchive() {
		String zipArchive = "scion-browser-" + BROWSER_VERSION + ".zip"; //$NON-NLS-1$ //$NON-NLS-2$
		return BrowserPlugin.class.getResourceAsStream(zipArchive);
	}

	/**
	 * Generate the built-in server's executable path, where we'd expect to find
	 * it relative to the build directory. Note that destDir is most likely the
	 * same as what {@link #builtinServerDirectoryPath
	 * builtinServerDirectoryPath} generated.
	 * 
	 * @param destDir
	 *            The destination directory where the built-in scion server is
	 *            being built.
	 */
	public static IPath serverExecutablePath(IPath destDir) {
		IPath exePath = destDir.append("dist").append("build").append("scion-browser"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		return exePath.append(FileUtil.makeExecutableName("scion-browser"));
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
}
