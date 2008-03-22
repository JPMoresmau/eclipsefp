// Copyright (c) 2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.ui.test.editor.HaskellConfiguration_Test;
import net.sf.eclipsefp.haskell.ui.test.editor.text.HaskellCharacterPairMatcher_Test;

/** collects all non-PDE test cases in this project.
  *
  * @author Leif Frenzel
  */
public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( HaskellCharacterPairMatcher_Test.class );
		suite.addTestSuite( HaskellConfiguration_Test.class );
		return suite;
	}
}
