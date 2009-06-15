// Copyright (c) 2009 by Thomas ten Cate <ttencate@gmail.com>
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.scion.client;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class ScionPlugin extends AbstractUIPlugin {
	
	private static final String BUNDLE_NAME = "net.sf.eclipsefp.haskell.scion.client";
	private static ScionPlugin instance;
	
	private ResourceBundle resourceBundle;
	
	public ScionPlugin() {
		instance = this;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		
		resourceBundle = ResourceBundle.getBundle("plugin");
		
		// Preload the server in anticipation of its use
		Scion.initializeClient();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		Scion.dispose();
		
		super.stop(context);
	}
	
	private static String getPluginId() {
		if (instance != null) {
			return instance.getBundle().getSymbolicName();
		} else {
			return BUNDLE_NAME; // fallback, but bad for mantainability...
		}
	}
	
	public static ScionPlugin getDefault() {
		return instance;
	}
	
	public String getString(String key) {
		try {
			return resourceBundle.getString(key);
		} catch (MissingResourceException ex) {
			return key;
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
