package net.sf.eclipsefp.haskell.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellCorePluginTest.class);
		//$JUnit-END$
		return suite;
	}

}
