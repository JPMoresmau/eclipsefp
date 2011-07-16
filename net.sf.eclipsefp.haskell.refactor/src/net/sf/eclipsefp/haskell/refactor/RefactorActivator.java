package net.sf.eclipsefp.haskell.refactor;

import net.sf.eclipsefp.haskell.refactor.module.ModuleNameChangeListener;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class RefactorActivator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.refactor"; //$NON-NLS-1$

	// The shared instance
	private static RefactorActivator plugin;
	
	private ModuleNameChangeListener moduleListener;
	
	/**
	 * The constructor
	 */
	public RefactorActivator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		
		moduleListener = new ModuleNameChangeListener();
		ResourcesPlugin.getWorkspace().addResourceChangeListener(moduleListener);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(moduleListener);
		
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static RefactorActivator getDefault() {
		return plugin;
	}

}
