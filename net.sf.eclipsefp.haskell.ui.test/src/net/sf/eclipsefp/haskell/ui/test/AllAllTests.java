package net.sf.eclipsefp.haskell.ui.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test");
		//$JUnit-BEGIN$
		suite.addTest( net.sf.eclipsefp.haskell.ui.test.editor.text.AllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
