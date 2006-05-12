package net.sf.eclipsefp.haskell.core.test.halamo;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.halamo");
		//$JUnit-BEGIN$
		suite.addTestSuite(LanguageModelTest.class);
		suite.addTestSuite(ScopeTest.class);
		//$JUnit-END$
		return suite;
	}

}
