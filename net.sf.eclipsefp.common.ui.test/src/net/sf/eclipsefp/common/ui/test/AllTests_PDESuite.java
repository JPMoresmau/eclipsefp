package net.sf.eclipsefp.common.ui.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.common.ui.test");
		suite.addTest(net.sf.eclipsefp.common.ui.wizards.AllTests_PDESuite.suite());
		//$JUnit-BEGIN$

		//$JUnit-END$
		return suite;
	}

}
