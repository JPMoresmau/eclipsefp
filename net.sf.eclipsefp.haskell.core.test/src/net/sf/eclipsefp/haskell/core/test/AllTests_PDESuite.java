package net.sf.eclipsefp.haskell.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.core.test.code.SourceFileGenerator_PDETest;
import net.sf.eclipsefp.haskell.core.test.compiler.CompilerManager_PDETest;
import net.sf.eclipsefp.haskell.core.test.expressions.HaskellPropertyTester_PDETest;
import net.sf.eclipsefp.haskell.core.test.halamo.HaskellModelManager_PDETest;
import net.sf.eclipsefp.haskell.core.test.halamo.ProjectChangeMonitor_PDETest;
import net.sf.eclipsefp.haskell.core.test.halamo.WorkspaceChangeMonitor_PDETest;
import net.sf.eclipsefp.haskell.core.test.internal.project.CabalBuilder_PDETest;
import net.sf.eclipsefp.haskell.core.test.internal.project.ProjectCreationOperation_PDETest;
import net.sf.eclipsefp.haskell.core.test.project.HaskellProjectCreationOperation_PDETest;
import net.sf.eclipsefp.haskell.core.test.project.ImportLibraries_PDETest;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( CabalBuilder_PDETest.class );
    suite.addTestSuite( CompilerManager_PDETest.class );
		suite.addTestSuite( SourceFileGenerator_PDETest.class );
		suite.addTestSuite( HaskellPropertyTester_PDETest.class );
		suite.addTestSuite( HaskellModelManager_PDETest.class );
		suite.addTestSuite( ProjectChangeMonitor_PDETest.class );
		suite.addTestSuite( WorkspaceChangeMonitor_PDETest.class );
		suite.addTestSuite( ProjectCreationOperation_PDETest.class );
		suite.addTestSuite( HaskellProjectCreationOperation_PDETest.class );
		suite.addTestSuite( ImportLibraries_PDETest.class );
		suite.addTestSuite( ProjectCreationOperation_PDETest.class );
		return suite;
	}

}
