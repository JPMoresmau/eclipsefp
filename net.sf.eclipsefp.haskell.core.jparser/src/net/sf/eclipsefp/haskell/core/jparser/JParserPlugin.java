package net.sf.eclipsefp.haskell.core.jparser;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class JParserPlugin extends Plugin {

	private static JParserPlugin plugin;

	public JParserPlugin() {
		plugin = this;
	}
	
	/**
	 * This method is called upon plug-in activation
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	/**
	 * Returns the shared instance.
	 */
	public static JParserPlugin getDefault() {
	    return plugin;
	}
	
	public static String getPluginId() {
		return getDefault().getBundle().getSymbolicName();
	}
}
