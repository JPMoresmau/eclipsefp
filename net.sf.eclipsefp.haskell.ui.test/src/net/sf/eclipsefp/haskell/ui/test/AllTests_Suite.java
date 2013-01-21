// Copyright (c) 2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.test;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellCharacterPairMatcher_Test;
import net.sf.eclipsefp.haskell.ui.internal.resolve.MissingTypeWarningTest;
import net.sf.eclipsefp.haskell.ui.internal.resolve.ResolveFromMessageTests;

/** collects all non-PDE test cases in this project.
  *
  * @author Leif Frenzel
  */
public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( HaskellCharacterPairMatcher_Test.class );

		suite.addTest( new JUnit4TestAdapter( MissingTypeWarningTest.class ) );
		suite.addTest( new JUnit4TestAdapter( ResolveFromMessageTests.class ) );
		return suite;
	}
}
