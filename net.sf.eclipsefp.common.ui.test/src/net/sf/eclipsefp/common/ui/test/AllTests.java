package net.sf.eclipsefp.common.ui.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.common.ui.test");
		suite.addTest(net.sf.eclipsefp.common.ui.wizards.AllTests.suite());
		//$JUnit-BEGIN$

		//$JUnit-END$
		return suite;
	}

}
