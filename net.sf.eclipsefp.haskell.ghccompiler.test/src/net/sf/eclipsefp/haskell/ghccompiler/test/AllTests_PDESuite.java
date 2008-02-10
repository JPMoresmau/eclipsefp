package net.sf.eclipsefp.haskell.ghccompiler.test;

import net.sf.eclipsefp.haskell.ghccompiler.test.core.GhcCompilerTest_PDETest;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.ghccompiler.test.core");
		//$JUnit-BEGIN$
		suite.addTestSuite(GhcCompilerTest_PDETest.class);
		//$JUnit-END$
		return suite;
	}

}
