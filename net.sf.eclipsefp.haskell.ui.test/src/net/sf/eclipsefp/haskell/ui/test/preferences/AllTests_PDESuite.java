package net.sf.eclipsefp.haskell.ui.test.preferences;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test.util.preferences");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellPreferenceManager_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
