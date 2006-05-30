package net.sf.eclipsefp.haskell.ui.test.editor;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test.editor");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellConfigurationTest.class);
		//$JUnit-END$
		return suite;
	}

}
