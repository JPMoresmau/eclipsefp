package net.sf.eclipsefp.haskell.core.test.halamo;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.halamo");
		//$JUnit-BEGIN$
		suite.addTestSuite(ResourceChangeMonitor_PDETestCase.class);
		suite.addTest(AllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
