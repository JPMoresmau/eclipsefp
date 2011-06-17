package net.sf.eclipsefp.haskell.browser;

import java.io.Writer;
import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.client.NullBrowserServer;
import net.sf.eclipsefp.haskell.browser.client.StreamBrowserServer;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class BrowserPlugin extends AbstractUIPlugin implements IDatabaseLoadedListener {
	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.browser"; //$NON-NLS-1$
	public static final String BROWSER_VERSION = "0.1";
	// The shared instance
	private static BrowserPlugin plugin;

	// The instance of the server used for communication
	private BrowserServer server;
	// The console log output
	private Writer logStream;
	
	// Listeners for loading new databases
	private ArrayList<IDatabaseLoadedListener> dbLoadedListeners;

	/**
	 * The constructor
	 */
	public BrowserPlugin() {
		this.server = new NullBrowserServer();
		this.logStream = null;
		this.dbLoadedListeners = new ArrayList<IDatabaseLoadedListener>();
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
	public static IPath builtinBrowserDirectoryPath() {
		IPath path = getDefault().getStateLocation().append(
				"scion-browser-".concat(BROWSER_VERSION)); //$NON-NLS-1$
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
	 * Raises an event for all listeners currently registered
	 * 
	 * @param e
	 */
	protected void notifyDatabaseLoaded(DatabaseLoadedEvent e) {
		for (IDatabaseLoadedListener listener : dbLoadedListeners)
			listener.databaseLoaded(e);
	}

	public void databaseLoaded(DatabaseLoadedEvent e) {
		notifyDatabaseLoaded(e);
	}
}
