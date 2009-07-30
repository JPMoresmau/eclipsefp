// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.core.compiler.ListenableCompilerDecorator_Test;
import net.sf.eclipsefp.haskell.core.halamo.LanguageModel_Test;
import net.sf.eclipsefp.haskell.core.halamo.Scope_Test;
import net.sf.eclipsefp.haskell.core.internal.util.MultiplexedWriter_Test;
import net.sf.eclipsefp.haskell.core.project.ImportLibrariesList_Test;
import net.sf.eclipsefp.haskell.core.util.ProcessRunner_Test;

public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( ImportLibrariesList_Test.class );
		suite.addTestSuite( LanguageModel_Test.class );
		suite.addTestSuite( ListenableCompilerDecorator_Test.class );
		suite.addTestSuite( MultiplexedWriter_Test.class );
		suite.addTestSuite( Scope_Test.class );
		suite.addTestSuite( ProcessRunner_Test.class );
		return suite;
	}

}
