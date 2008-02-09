package net.sf.eclipsefp.haskell.core.test;

import net.sf.eclipsefp.haskell.core.test.internal.util.MultiplexedWriter_Test;
import net.sf.eclipsefp.haskell.core.test.internal.util.ResourceUtil_Test;
import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( ResourceUtil_Test.class );
		suite.addTestSuite( MultiplexedWriter_Test.class );
		return suite;
	}

}
