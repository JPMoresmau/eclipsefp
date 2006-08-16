package net.sf.eclipsefp.common.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.common.core.test");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.common.core.test.util.AllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
