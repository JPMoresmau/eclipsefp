/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.ui.internal.preferences.HaskellPreferenceManager_PDETest;
import net.sf.eclipsefp.haskell.ui.internal.preferences.HaskellPreferenceProvider_PDETest;
import net.sf.eclipsefp.haskell.ui.test.console.ConsoleCompilingReporter_PDETest;
import net.sf.eclipsefp.haskell.ui.test.editor.HaskellDocumentProvider_PDETest;
import net.sf.eclipsefp.haskell.ui.test.editor.Partitioning_PDETest;
import net.sf.eclipsefp.haskell.ui.test.editor.SyntaxColoring_PDETest;
import net.sf.eclipsefp.haskell.ui.test.editor.codeassist.CompletionContext_PDETest;
import net.sf.eclipsefp.haskell.ui.test.editor.codeassist.HaskellContentAssistProcessor_PDETest;
import net.sf.eclipsefp.haskell.ui.test.editor.codeassist.WorkbenchHaskellCompletionContext_PDETest;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui");

		suite.addTestSuite( ConsoleCompilingReporter_PDETest.class );
		suite.addTestSuite( HaskellPreferenceProvider_PDETest.class );
    suite.addTestSuite( HaskellPreferenceManager_PDETest.class );
    suite.addTestSuite( HaskellDocumentProvider_PDETest.class );

    // coloring
    suite.addTestSuite( Partitioning_PDETest.class );
    suite.addTestSuite( SyntaxColoring_PDETest.class );

    // code assist
    suite.addTestSuite( HaskellContentAssistProcessor_PDETest.class );
    suite.addTestSuite( WorkbenchHaskellCompletionContext_PDETest.class );
    suite.addTestSuite( CompletionContext_PDETest.class );

		return suite;
	}

}
