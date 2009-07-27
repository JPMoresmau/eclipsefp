package net.sf.eclipsefp.haskell.core.jparser.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class MainTests {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		//$JUnit-BEGIN$
		suite.addTestSuite(ParserIntegrationTest.class);
		suite.addTestSuite(LexerTest.class);
		suite.addTestSuite(LiterateHaskellReaderTest.class);
		suite.addTestSuite(QualifiedIdentifierFilterTest.class);
		suite.addTestSuite(ModuleBuilderTest.class);
		suite.addTestSuite(ParserUnitTest.class);
		suite.addTestSuite(LookaheadTokenStreamTest.class);
		suite.addTestSuite(PreprocessedTokenStreamTest.class);
		suite.addTestSuite(FormatterTest.class);
		//$JUnit-END$
		return suite;
	}

}
