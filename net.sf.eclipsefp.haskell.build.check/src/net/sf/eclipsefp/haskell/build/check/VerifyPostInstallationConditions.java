package net.sf.eclipsefp.haskell.build.check;

import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.core.runtime.Platform;

public class VerifyPostInstallationConditions implements IPlatformRunnable {

	public Object run(Object args) throws Exception {
		try {
			checkPluginExistence("de.leiffrenzel.fp.haskell.core");
			checkSourceCodePlugin("de.leiffrenzel.fp.haskell.source");
			return 0;
		} catch (Exception e) {
			final String message = "Bad installation. " + e.getMessage();
			System.err.println(message);
			return -1;
		}
	}

	private void checkPluginExistence(final String pluginName) throws Exception {
		Object bundle = Platform.getBundle(pluginName);
		if (bundle == null) {
			throw new Exception(pluginName + " plugin not found.");
		}
	}

	private void checkSourceCodePlugin(final String pluginName) throws Exception {
		IExtensionRegistry reg = Platform.getExtensionRegistry();
		IExtensionPoint point = reg.getExtensionPoint("org.eclipse.pde.core.source");
		for (IExtension ext : point.getExtensions()) {
			if (pluginName.equals(ext.getNamespace())) {
				return;
			}
		}
		throw new Exception("Source code plugin " + pluginName + " not found");
	}

}
