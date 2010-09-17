// Copyright (c) 2009 by Thomas ten Cate <ttencate@gmail.com>
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.scion.client;

import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.BundleContext;

public class ScionPlugin extends AbstractUIPlugin {
	
	private static final String BUNDLE_NAME = ScionPlugin.class.getCanonicalName();
	private static ScionPlugin instance;
	public static final String ID_PROJECT_PROBLEM_MARKER = BUNDLE_NAME+".projectProblem"; //$NON-NLS-1$

	// no dot so we see it in Eclipse project view
	public static final String DIST_FOLDER="dist-scion";
	
	public static final String SCION_VERSION="0.1.0.4";
	
	private final Map<IProject, ScionInstance> instances = new HashMap<IProject, ScionInstance>();

	
	/**
	 * The version number of the Scion protocol that we support.
	 */
	public static final int PROTOCOL_VERSION = 1;
	
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
	 */
	public static boolean isTracing(String optionId) {
		String option = getPluginId() + "/" + optionId;
	    String value = Platform.getDebugOption(option);
	    return value != null && value.equalsIgnoreCase("true");
	}
	
	public static void logInfo (String message) {
		log(Status.INFO, message, null);
	}
	public static void logWarning(String message, Throwable cause) {
		log(Status.WARNING, message, cause);
	}
	
	public static void logWarning(ScionCommand command, String message, Throwable cause) {
		log(Status.WARNING, message + "\n" + command.getErrorInfo(), cause);
	}
	
	public static void logError(String message, Throwable cause) {
		log(Status.ERROR, message, cause);
	}
	
	public static void log(int severity, String message, Throwable cause) {
		Status status = new Status(severity, ScionPlugin.getPluginId(), severity, message, cause);
		logStatus(status);
	}
	
	public static void logStatus(IStatus status) {
		StatusManager.getManager().handle(status);
	}
	

	  
	  public Map<IProject, ScionInstance> getScionInstances() {
		return instances;
	}
}
