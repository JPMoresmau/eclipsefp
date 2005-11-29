package net.sf.eclipsefp.haskell.core.jparser.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"JParser plugin full test suit");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.haskell.core.jparser.test.AllTests.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.jparser.ast.test.AllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
