package net.sf.eclipsefp.haskell.core.test.compiler;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.compiler");
		//$JUnit-BEGIN$
		suite.addTestSuite(CompilerManagerTest.class);
		suite.addTestSuite(CompilerOutputItemTest.class);
		suite.addTestSuite(ListenableCompilerDecoratorTest.class);
		//$JUnit-END$
		return suite;
	}

}
