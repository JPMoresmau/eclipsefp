// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.core.test.compiler.CompilerOutputItem_Test;
import net.sf.eclipsefp.haskell.core.test.compiler.ListenableCompilerDecorator_Test;
import net.sf.eclipsefp.haskell.core.test.halamo.LanguageModel_Test;
import net.sf.eclipsefp.haskell.core.test.halamo.Scope_Test;
import net.sf.eclipsefp.haskell.core.test.internal.project.provisionary.MarkerDesc_Test;
import net.sf.eclipsefp.haskell.core.test.internal.util.MultiplexedWriter_Test;
import net.sf.eclipsefp.haskell.core.test.project.ImportLibrariesList_Test;

public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( CompilerOutputItem_Test.class );
		suite.addTestSuite( ListenableCompilerDecorator_Test.class );
		suite.addTestSuite( MultiplexedWriter_Test.class );
		suite.addTestSuite( MarkerDesc_Test.class );
		suite.addTestSuite( LanguageModel_Test.class );
		suite.addTestSuite( Scope_Test.class );
		suite.addTestSuite( ImportLibrariesList_Test.class );
		return suite;
	}

}
