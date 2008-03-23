package net.sf.eclipsefp.haskell.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.core.code.SourceFileGenerator_PDETest;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager_PDETest;
import net.sf.eclipsefp.haskell.core.expressions.HaskellPropertyTester_PDETest;
import net.sf.eclipsefp.haskell.core.halamo.HaskellModelManager_PDETest;
import net.sf.eclipsefp.haskell.core.halamo.ProjectChangeMonitor_PDETest;
import net.sf.eclipsefp.haskell.core.halamo.WorkspaceChangeMonitor_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.CabalBuilder_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.ManipulateCabalFile_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectModel_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.TestHaskellProjectMetaTest_PDETest;
import net.sf.eclipsefp.haskell.core.internal.util.ResourceUtil_PDETest;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation_PDETest;
import net.sf.eclipsefp.haskell.core.project.HaskellResource_PDETest;
import net.sf.eclipsefp.haskell.core.project.ImportLibraries_PDETest;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( CabalBuilder_PDETest.class );
    suite.addTestSuite( CompilerManager_PDETest.class );
		suite.addTestSuite( HaskellModelManager_PDETest.class );
		suite.addTestSuite( HaskellProject_PDETest.class );
		suite.addTestSuite( HaskellProjectCreationOperation_PDETest.class );
		suite.addTestSuite( HaskellPropertyTester_PDETest.class );
		suite.addTestSuite( HaskellResource_PDETest.class );
		suite.addTestSuite( ImportLibraries_PDETest.class );
		suite.addTestSuite( ManipulateCabalFile_PDETest.class );
		suite.addTestSuite( ProjectChangeMonitor_PDETest.class );
		suite.addTestSuite( ProjectCreationOperation_PDETest.class );
		suite.addTestSuite( ProjectModel_PDETest.class );
    suite.addTestSuite( ResourceUtil_PDETest.class );
    suite.addTestSuite( SourceFileGenerator_PDETest.class );
    suite.addTestSuite( TestHaskellProjectMetaTest_PDETest.class );
		suite.addTestSuite( WorkspaceChangeMonitor_PDETest.class );
		return suite;
	}

}
