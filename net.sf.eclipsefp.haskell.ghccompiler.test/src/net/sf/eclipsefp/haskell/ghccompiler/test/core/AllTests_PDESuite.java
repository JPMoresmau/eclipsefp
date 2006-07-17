package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ghccompiler.test.core");
		//$JUnit-BEGIN$
		suite.addTestSuite(GhcCompilerTest_PDETestCase.class);
		//$JUnit-END$
		return suite;
	}

}
