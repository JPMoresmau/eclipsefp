package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for net.sf.eclipsefp.haskell.core.jparser.ast.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(FunctionBindingTest.class);
		suite.addTestSuite(DataDeclarationTest.class);
		suite.addTestSuite(TypeSignatureTest.class);
		suite.addTestSuite(SourceLocationTest.class);
		suite.addTestSuite(ClassDeclarationTest.class);
		suite.addTestSuite(ModuleTest.class);
		//$JUnit-END$
		return suite;
	}

}
