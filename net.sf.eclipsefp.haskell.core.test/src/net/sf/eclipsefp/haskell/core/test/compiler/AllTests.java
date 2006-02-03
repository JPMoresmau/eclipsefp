package net.sf.eclipsefp.haskell.core.test.compiler;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.compiler");
		//$JUnit-BEGIN$
		suite.addTestSuite(CompilerOutputItemTest.class);
		//$JUnit-END$
		return suite;
	}

}
