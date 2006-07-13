package net.sf.eclipsefp.haskell.ui.test.editor;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test.editor");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellDocumentProvider_PDETestCase.class);
		//$JUnit-END$
		return suite;
	}

}
