package net.sf.eclipsefp.haskell.hlint;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class HLintPlugin extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.hlint"; //$NON-NLS-1$

	// The shared instance
	private static HLintPlugin plugin;
	
	private static String hlintPath;
	
	/**
	 * The constructor
	 */
	public HLintPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static HLintPlugin getDefault() {
		return plugin;
	}

	  public static void logError(String message, Throwable cause) {
	    log(Status.ERROR, message, cause);
	  }

	  public static void log(int severity, String message, Throwable cause) {
	    Status status = new Status(severity, HLintPlugin.PLUGIN_ID, severity, message, cause);
	    logStatus(status);
	  }

	  public static void logStatus(IStatus status) {
	    StatusManager.getManager().handle(status);
	  }

	public static String getHlintPath() {
		return hlintPath;
	}

	public static void setHlintPath(String hlintPath) {
		HLintPlugin.hlintPath = hlintPath;
	}
}
