package net.sf.eclipsefp.haskell.core.test;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.core.code.SourceFileGenerator_PDETest;
import net.sf.eclipsefp.haskell.core.expressions.HaskellPropertyTester_PDETest;
import net.sf.eclipsefp.haskell.core.internal.contenttypes.ContentTypes_PDETest;
import net.sf.eclipsefp.haskell.core.internal.contenttypes.LiterateContentDescriber_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.CabalBuilder_PDETest;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation_PDETest;
import net.sf.eclipsefp.haskell.core.internal.util.ResourceUtil_PDETest;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation_PDETest;
import net.sf.eclipsefp.haskell.core.project.HaskellResource_PDETest;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(AllTests_PDESuite.class.getName());
		suite.addTestSuite( CabalBuilder_PDETest.class );
    suite.addTestSuite( ContentTypes_PDETest.class );
		suite.addTestSuite( HaskellProjectCreationOperation_PDETest.class );
		suite.addTestSuite( HaskellPropertyTester_PDETest.class );
		suite.addTestSuite( HaskellResource_PDETest.class );
		suite.addTestSuite( LiterateContentDescriber_PDETest.class );
		suite.addTestSuite( ProjectCreationOperation_PDETest.class );
    suite.addTestSuite( ResourceUtil_PDETest.class );
    suite.addTestSuite( SourceFileGenerator_PDETest.class );

		Assert.assertTrue(suite instanceof Test);
		return suite;
	}

}
