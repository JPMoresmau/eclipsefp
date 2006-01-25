package net.sf.eclipsefp.haskell.core.test.code;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.test.code");
		//$JUnit-BEGIN$
		suite.addTestSuite(SourceFileGenerator_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
