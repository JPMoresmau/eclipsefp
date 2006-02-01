package net.sf.eclipsefp.haskell.build.check;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.core.runtime.Platform;

public class VerifyPostInstallationConditions implements IPlatformRunnable {

	public Object run(Object args) throws Exception {
		try {
			checkPluginExistence("de.leiffrenzel.fp.haskell.core");
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
			throw new Exception("Haskell core plugin not found.");
		}
	}

}
