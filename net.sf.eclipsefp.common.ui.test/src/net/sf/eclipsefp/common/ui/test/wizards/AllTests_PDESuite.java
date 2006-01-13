package net.sf.eclipsefp.common.ui.test.wizards;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.common.ui.wizards");
		//$JUnit-BEGIN$
		suite.addTestSuite(ProjectCreationOperation_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
