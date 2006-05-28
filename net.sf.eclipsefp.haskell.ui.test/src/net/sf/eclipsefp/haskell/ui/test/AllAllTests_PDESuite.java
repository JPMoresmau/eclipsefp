package net.sf.eclipsefp.haskell.ui.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.haskell.ui.test.util.preferences.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ui.test.editor.codeassist.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ui.test.AllAllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
