package net.sf.eclipsefp.haskell.ui.test;

import net.sf.eclipsefp.haskell.ui.test.editor.HaskellConfiguration_Test;
import net.sf.eclipsefp.haskell.ui.test.editor.text.HaskellCharacterPairMatcher_Test;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test");
		//$JUnit-BEGIN$
		suite.addTestSuite( HaskellCharacterPairMatcher_Test.class );
		suite.addTestSuite( HaskellConfiguration_Test.class );
		//$JUnit-END$
		return suite;
	}

}
