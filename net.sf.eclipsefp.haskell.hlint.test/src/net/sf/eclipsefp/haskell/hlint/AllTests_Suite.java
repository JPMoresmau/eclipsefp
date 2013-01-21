/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.hlint;

import net.sf.eclipsefp.haskell.hlint.parser.OutputParserTest;
import junit.framework.JUnit4TestAdapter;
import junit.framework.TestSuite;

/**
 * @author JP Moresmau
 *
 */
public class AllTests_Suite {

	public static TestSuite suite(){
		TestSuite suite=new TestSuite();
		suite.addTest(new JUnit4TestAdapter(HLintFixerTest.class));
		suite.addTest(new JUnit4TestAdapter(OutputParserTest.class));
		
		return suite;
	}
}
