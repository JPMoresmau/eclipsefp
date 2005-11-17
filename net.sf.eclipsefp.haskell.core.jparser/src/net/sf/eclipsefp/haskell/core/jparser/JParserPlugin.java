package net.sf.eclipsefp.haskell.core.jparser;

import org.eclipse.core.runtime.Plugin;

public class JParserPlugin extends Plugin {

	private static JParserPlugin plugin;

	public JParserPlugin() {
		plugin = this;
	}
	
	public static JParserPlugin getDefault() {
	    return plugin;
	}
	

	public static String getPluginId() {
		return getDefault().getBundle().getSymbolicName();
	}
}
