package net.sf.eclipsefp.haskell.build.check;

import org.eclipse.core.runtime.Platform;
import org.eclipse.equinox.app.IApplication;
import org.eclipse.equinox.app.IApplicationContext;

/**
 * This program verifies some precontions that need to be met by the build
 * driver platform before starting to build.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class VerifyPreconditions implements IApplication {

  
  // interface methods of IApplication
  ////////////////////////////////////
  
	public Object start( final IApplicationContext context ) throws Exception {
	  int result = 0;
		Object bundle = Platform.getBundle("net.sf.eclipsefp.haskell.core");
		if (bundle != null) {
			System.err.println("EclipseFP plugins found. Please try to build using a platform _without_ EclipseFP installed.");
			result = -127;
		}
		return result;
	}

	public void stop() {
	  // unused
	}
}
