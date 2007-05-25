package net.sf.eclipsefp.haskell.core.test.expressions;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Tests for net.sf.eclipsefp.haskell.core.test.expressions");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellPropertyTesterTest.class);
		//$JUnit-END$
		return suite;
	}

}
