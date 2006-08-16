package net.sf.eclipsefp.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for net.sf.eclipsefp.test");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.haskell.core.jparser.test.AllAllTests.suite());
		suite.addTest(net.sf.eclipsefp.haskell.core.test.AllAllTests.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ghccompiler.test.AllAllTests.suite());
		suite.addTest(net.sf.eclipsefp.haskell.ui.test.AllAllTests.suite());
		suite.addTest(net.sf.eclipsefp.common.core.test.AllAllTests.suite());
		//$JUnit-END$
		return suite;
	}

}
