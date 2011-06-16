package net.sf.eclipsefp.haskell.browser;

import java.io.Writer;

import net.sf.eclipsefp.haskell.browser.client.BrowserServer;
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
public class BrowserPlugin extends AbstractUIPlugin {
	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.browser"; //$NON-NLS-1$
	public static final String BROWSER_VERSION = "0.1";
	// The shared instance
	private static BrowserPlugin plugin;

	// The instance of the server used for communication
	private BrowserServer server;
	// The console log output
	private Writer logStream;

	/**
	 * The constructor
	 */
	public BrowserPlugin() {
		this.server = new NullBrowserServer();
		this.logStream = null;
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

	public static BrowserServer getSharedInstance() {
		return getDefault().server;
	}

	public void setLogStream(Writer logStream) {
		this.logStream = logStream;
		this.server.setLogStream(logStream);
	}

	public static void setSharedLogStream(Writer logStream) {
		getDefault().setLogStream(logStream);
	}

	public void changeInstance(IPath path) {
		// Destroy previous scion-browser
		if (this.server != null)
			this.server.stop();
		
		if (path.toFile().exists()) {
			try {
				this.server = new StreamBrowserServer(path);
				this.server.setLogStream(this.logStream);
			} catch (Throwable ex) {
				this.server = new NullBrowserServer();
			}
		} else {
			this.server = new NullBrowserServer();
		}
	}

	public static void changeSharedInstance(IPath path) {
		getDefault().changeInstance(path);
	}

	public void useNullInstance() {
		// Destroy previous scion-browser
		if (this.server != null)
			this.server.stop();
		
		this.server = new NullBrowserServer();
	}

	public static void useNullSharedInstance() {
		getDefault().useNullInstance();
	}

	public static IStatus loadLocalDatabase(boolean rebuild) {
		try {
			getSharedInstance().loadLocalDatabase(getLocalDatabasePath().toOSString(), rebuild);
			return Status.OK_STATUS;
		} catch (Throwable ex) {
			return new Status(Status.ERROR, PLUGIN_ID, "", ex);
		}
	}

	public static IPath builtinBrowserDirectoryPath() {
		IPath path = getDefault().getStateLocation().append(
				"scion-browser-".concat(BROWSER_VERSION)); //$NON-NLS-1$
		if (!path.toFile().exists())
			path.toFile().mkdirs();
		return path;
	}

	public static IPath getLocalDatabasePath() {
		return builtinBrowserDirectoryPath().append("local.db");
	}

	public static IPath getHackageDatabasePath() {
		return builtinBrowserDirectoryPath().append("hackage.db");
	}
}
