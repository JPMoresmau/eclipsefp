package net.sf.eclipsefp.haskell.core.test.codeassist;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.codeassist");
		//$JUnit-BEGIN$
		suite.addTestSuite(CompletionEngine_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
