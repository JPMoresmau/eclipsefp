package net.sf.eclipsefp.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTest(net.sf.eclipsefp.haskell.core.test.AllTests_Suite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ghccompiler.test.AllTests_Suite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ui.test.AllTests_Suite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.scion.client.test.AllTests_Suite.suite());
		return suite;
	}
	
}
