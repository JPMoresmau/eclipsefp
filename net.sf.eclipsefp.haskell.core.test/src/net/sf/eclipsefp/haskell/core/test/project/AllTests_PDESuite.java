package net.sf.eclipsefp.haskell.core.test.project;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.project");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellProjectCreationOperation_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
