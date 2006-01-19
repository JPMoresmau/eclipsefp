package net.sf.eclipsefp.haskell.ui.test;

import net.sf.eclipsefp.haskell.ui.test.util.preferences.AllTests_PDESuite;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllAllTests_PDESuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for de.leiffrenzel.fp.haskell.ui");
		//$JUnit-BEGIN$
		suite.addTest(AllTests_PDESuite.suite());
		//$JUnit-END$
		return suite;
	}

}
