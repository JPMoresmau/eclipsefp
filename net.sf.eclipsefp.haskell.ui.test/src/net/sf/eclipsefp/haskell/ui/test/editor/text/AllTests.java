package net.sf.eclipsefp.haskell.ui.test.editor.text;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test.editor.text");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellCharacterPairMatcherTest.class);
		//$JUnit-END$
		return suite;
	}

}
