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
	@Override
  public void start(final BundleContext context) throws Exception {
		super.start(context);
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	@Override
  public void stop(final BundleContext context) throws Exception {
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
