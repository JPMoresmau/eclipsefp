package net.sf.eclipsefp.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for net.sf.eclipsefp.test");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.haskell.core.jparser.test.AllAllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
