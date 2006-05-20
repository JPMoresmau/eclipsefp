package net.sf.eclipsefp.haskell.ui.test.editor.codeassist;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ui.test.editor.codeassist");
		//$JUnit-BEGIN$
		suite.addTestSuite(HaskellContentAssistProcessor_PDETest.class);
		suite.addTestSuite(WorkbenchHaskellCompletionContext_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
