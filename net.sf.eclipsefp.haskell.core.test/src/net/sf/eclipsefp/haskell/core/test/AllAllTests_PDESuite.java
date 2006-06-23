package net.sf.eclipsefp.haskell.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.haskell.core.test.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.code.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.codeassist.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.compiler.AllTests.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.halamo.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.project.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.util.AllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
