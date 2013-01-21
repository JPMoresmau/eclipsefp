/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author JP Moresmau
 *
 */
public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( DispatchWriterTest.class );

		suite.addTest( new JUnit4TestAdapter(CommandLineUtilTest.class ));
		suite.addTestSuite( FileUtilTest.class );
		return suite;
	}
}
