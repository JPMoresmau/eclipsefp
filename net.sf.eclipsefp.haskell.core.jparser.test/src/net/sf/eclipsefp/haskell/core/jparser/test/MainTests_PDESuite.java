package net.sf.eclipsefp.haskell.core.jparser.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class MainTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		//$JUnit-BEGIN$
		suite.addTestSuite(ParserPlugin_PDETest.class);
		suite.addTest(MainTests.suite());
		//$JUnit-END$
		return suite;
	}

}
