package net.sf.eclipsefp.haskell.ui.test.wizards;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test.wizards");
		//$JUnit-BEGIN$
		suite.addTestSuite(ModuleCreatioOperation_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
