/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.BundleContext;

/**
 * Style plugin
 * @author JP Moresmau
 *
 */
public class StylePlugin extends AbstractUIPlugin {
	private static StylePlugin plugin=null;

	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.style";
	
	/**
	 * @return the plugin
	 */
	public static StylePlugin getStylePlugin() {
		return plugin;
	}

	 public static void logError(Throwable cause) {
	    log(Status.ERROR, cause.getLocalizedMessage(), cause);
	  }
	
	 public static void logError(String message, Throwable cause) {
	    log(Status.ERROR, message, cause);
	  }

	  public static void log(int severity, String message, Throwable cause) {
	    Status status = new Status(severity, PLUGIN_ID, severity, message, cause);
	    logStatus(status);
	  }

	  public static void logStatus(IStatus status) {
	    StatusManager.getManager().handle(status);
	  }
	  

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext bundleContext) throws Exception {
		super.start(bundleContext);
		plugin=this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext bundleContext) throws Exception {
		super.stop(bundleContext);
		plugin=null;
	}

}
