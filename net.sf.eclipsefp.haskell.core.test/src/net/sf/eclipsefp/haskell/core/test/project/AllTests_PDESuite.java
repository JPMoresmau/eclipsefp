package net.sf.eclipsefp.haskell.core.test.project;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.core.test.internal.project.ProjectCreationOperation_PDETest;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( HaskellProjectCreationOperation_PDETest.class );
		suite.addTestSuite( ImportLibraries_PDETest.class );
		suite.addTestSuite( ProjectCreationOperation_PDETest.class );
		suite.addTest(AllTests.suite());
		return suite;
	}

}
