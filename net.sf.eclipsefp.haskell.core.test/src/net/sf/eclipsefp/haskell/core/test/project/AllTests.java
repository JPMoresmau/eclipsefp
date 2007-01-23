package net.sf.eclipsefp.haskell.core.test.project;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.project");
		//$JUnit-BEGIN$
		suite.addTestSuite(ImportLibrariesListTest.class);
		//$JUnit-END$
		return suite;
	}

}
