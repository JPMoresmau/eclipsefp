package net.sf.eclipsefp.test.util.haskell.metatest;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllMetaTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.test.util.haskell.metatest");
		//$JUnit-BEGIN$
		suite.addTestSuite(TestHaskellProjectMetaTest_PDETestCase.class);
		//$JUnit-END$
		return suite;
	}

}
