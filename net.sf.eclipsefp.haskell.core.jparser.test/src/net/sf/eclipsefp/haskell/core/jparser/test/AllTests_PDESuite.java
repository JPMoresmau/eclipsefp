package net.sf.eclipsefp.haskell.core.jparser.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.jparser.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(ParserPlugin_PDETest.class);
		suite.addTest(AllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
