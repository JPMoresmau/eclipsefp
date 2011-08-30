package net.sf.eclipsefp.haskell.profiler;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class ProfilerPlugin extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.profiler"; //$NON-NLS-1$

	// The shared instance
	private static ProfilerPlugin plugin;
	
	/**
	 * The constructor
	 */
	public ProfilerPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static ProfilerPlugin getDefault() {
		return plugin;
	}
	

	  public static void log(int severity, String message, Throwable cause) {
	    Status status = new Status(severity, PLUGIN_ID, severity, message, cause);
	    logStatus(status);
	  }
	
	  public static void logStatus(IStatus status) {
	    StatusManager.getManager().handle(status);
	  }

}
