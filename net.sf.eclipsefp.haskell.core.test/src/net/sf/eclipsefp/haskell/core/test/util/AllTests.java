package net.sf.eclipsefp.haskell.core.test.util;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.util");
		//$JUnit-BEGIN$
		suite.addTestSuite(ResourceUtilTest.class);
		//$JUnit-END$
		return suite;
	}

}
