package net.sf.eclipsefp.haskell.ui.test.editor;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllNonPDETests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test.editor");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellConfiguration_NonPDETest.class);
		//$JUnit-END$
		return suite;
	}

}
