package net.sf.eclipsefp.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for net.sf.eclipsefp.test");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.common.core.test.AllAllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.parser.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.jparser.test.AllAllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ui.test.AllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.AllAllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.test.util.AllAllTests_PDESuite.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ghccompiler.test.AllAllTests_PDESuite.suite());
		//$JUnit-END$
		return suite;
	}

}
