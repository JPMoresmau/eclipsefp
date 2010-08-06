package net.sf.eclipsefp.haskell.scion.client.test;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * 
 * @author JP Moresmau
 *
 */
public class AllTests_Suite {
	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTest(new JUnit4TestAdapter(JSONNavigatorTest.class ));
		return suite;
	}
}
