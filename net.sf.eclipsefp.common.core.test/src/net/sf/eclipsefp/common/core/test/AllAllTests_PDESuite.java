package net.sf.eclipsefp.common.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.common.core.test");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.common.core.test.project.AllTests_PDESuite.suite());
		//$JUnit-END$
		return suite;
	}

}
