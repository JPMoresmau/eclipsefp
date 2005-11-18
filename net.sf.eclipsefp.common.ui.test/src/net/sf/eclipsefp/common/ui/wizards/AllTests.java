package net.sf.eclipsefp.common.ui.wizards;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.common.ui.wizards");
		//$JUnit-BEGIN$
		suite.addTestSuite(ProjectCreationOperationTest.class);
		//$JUnit-END$
		return suite;
	}

}
