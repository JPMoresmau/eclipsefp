package net.sf.eclipsefp.haskell.build.check;

import org.eclipse.core.runtime.IPlatformRunnable;
import org.eclipse.core.runtime.Platform;

/**
 * This program verifies some precontions that need to be met by the build
 * driver platform before starting to build.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class VerifyPreconditions implements IPlatformRunnable {

	public Object run(Object args) throws Exception {
		Object bundle = Platform.getBundle("de.leiffrenzel.fp.haskell.core");
		if (bundle != null) {
			System.err.println("EclipseFP plugins found. Please try to build using a platform _without_ EclipseFP installed.");
			return -127;
		} else {
			return 0;
		}
	}
	
}
