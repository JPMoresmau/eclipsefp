package net.sf.eclipsefp.common.core.test.util;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.common.core.test.util");
		//$JUnit-BEGIN$
		suite.addTestSuite(StreamMultiplexerTest.class);
		//$JUnit-END$
		return suite;
	}

}
