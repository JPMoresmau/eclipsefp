package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.jparser.ast.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(FunctionBindingTest.class);
		//$JUnit-END$
		return suite;
	}

}
