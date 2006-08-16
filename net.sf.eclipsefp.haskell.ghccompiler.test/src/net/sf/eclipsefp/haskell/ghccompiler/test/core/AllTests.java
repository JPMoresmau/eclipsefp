package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ghccompiler.test.core");
		//$JUnit-BEGIN$
		suite.addTestSuite(GhcOutputParserTest.class);
		suite.addTestSuite(ProcessRunnerTest.class);
		//$JUnit-END$
		return suite;
	}

}
