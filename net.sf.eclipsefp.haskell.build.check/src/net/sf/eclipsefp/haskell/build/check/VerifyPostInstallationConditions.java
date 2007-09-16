package net.sf.eclipsefp.haskell.build.check;

import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.core.runtime.Platform;

public class VerifyPostInstallationConditions implements IPlatformRunnable {

	public Object run(Object args) throws Exception {
		try {
			assertPluginExistence("net.sf.eclipsefp.haskell.core");
			assertSourceCodePlugin("net.sf.eclipsefp.haskell.source");
			assertDocumentationPlugin("net.sf.eclipsefp.common.doc");
			assertDocumentationPlugin("net.sf.eclipsefp.haskell.doc.user");
			return 0;
		} catch (Exception e) {
			final String message = "Bad installation. " + e.getMessage();
			System.err.println(message);
			return -1;
		}
	}

	private void assertPluginExistence(final String pluginName) throws Exception {
		Object bundle = Platform.getBundle(pluginName);
		if (bundle == null) {
			throw new Exception(pluginName + " plugin not found.");
		}
	}

	private void assertSourceCodePlugin(final String pluginName) throws Exception {
		if (!checkDeclaredExtension(pluginName, "org.eclipse.pde.core.source")) {
			throw new Exception("Source code plugin " + pluginName + " not found");
		}
	}

	private void assertDocumentationPlugin(final String pluginName) throws Exception {
		if (!checkDeclaredExtension(pluginName, "org.eclipse.help.toc")) {
			throw new Exception("Documentation plugin " + pluginName + " not found");
		}
	}

	/**
	 * Checks if a given plugin declares an specific extension.
	 * 
	 * @param declaringPlugin the plugin that declares the extension
	 * @param extension the declared extension
	 * @return
	 */
	private boolean checkDeclaredExtension(final String declaringPlugin, String extension) {
		IExtensionRegistry reg = Platform.getExtensionRegistry();
		IExtensionPoint point = reg.getExtensionPoint(extension);
		for (IExtension ext : point.getExtensions()) {
			if (declaringPlugin.equals(ext.getNamespace())) {
				return true;
			}
		}
		return false;
	}
}
