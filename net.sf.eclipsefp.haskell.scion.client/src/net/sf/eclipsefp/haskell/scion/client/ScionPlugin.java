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
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}
	
	public static String getPluginId() {
		if (instance != null) {
			return instance.getBundle().getSymbolicName();
		} else {
			return BUNDLE_NAME; // fallback, but bad for mantainability...
		}
	}
	
	public static ScionPlugin getDefault() {
		return instance;
	}
	
	public static String getStringResource(String key) {
		ScionPlugin p = getDefault();
		if (p != null) {
			try {
				return p.resourceBundle.getString(key);
			} catch (MissingResourceException ex) {
				return key;
			}
		} else {
			// plugin still loading
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
