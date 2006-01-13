package net.sf.eclipsefp.haskell.core.jparser.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"JParser plugin full test suit");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.haskell.core.jparser.test.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.jparser.ast.test.AllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
