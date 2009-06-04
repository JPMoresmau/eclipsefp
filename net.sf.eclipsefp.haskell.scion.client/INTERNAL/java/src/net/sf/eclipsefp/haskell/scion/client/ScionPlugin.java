// Copyright (c) 2009 by Thomas ten Cate <ttencate@gmail.com>
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.scion.client;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class ScionPlugin extends Plugin {
	
	private static ScionPlugin instance;
	
	public ScionPlugin() {
		instance = this;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		
		// Preload the server in anticipation of its use
		ScionClient.initializeServer();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		ScionClient.dispose();
		
		super.stop(context);
	}
	
	private static String getPluginId() {
		if (instance != null) {
			return instance.getBundle().getSymbolicName();
		} else {
			return "net.sf.eclipsefp.haskell.scion.client"; // fallback, but bad for mantainability...
		}
	}
	
	/**
	 * Returns whether tracing is enabled for the specified option.
	 * TODO this is executed for every trace call, even when tracing is off... optimize?
	 */
	public static boolean isTracing(String optionId) {
		String option = getPluginId() + "/" + optionId;
	    String value = Platform.getDebugOption(option);
	    return value != null && value.equalsIgnoreCase("true");
	}

}
