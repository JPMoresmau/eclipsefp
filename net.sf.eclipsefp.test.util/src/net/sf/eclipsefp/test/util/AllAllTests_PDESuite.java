package net.sf.eclipsefp.test.util;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for net.sf.eclipsefp.test.util");
		//$JUnit-BEGIN$
		suite.addTest(net.sf.eclipsefp.test.util.haskell.metatest.AllMetaTests_PDESuite.suite());
		//$JUnit-END$
		return suite;
	}

}
