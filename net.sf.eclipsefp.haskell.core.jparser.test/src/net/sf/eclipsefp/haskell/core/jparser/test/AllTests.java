package net.sf.eclipsefp.haskell.core.jparser.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.jparser.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(ParserTest.class);
		suite.addTestSuite(LookaheadTokenStreamTest.class);
		suite.addTestSuite(LexerTest.class);
		suite.addTestSuite(PluginTest.class);
		suite.addTestSuite(FormatterTest.class);
		//$JUnit-END$
		return suite;
	}

}
