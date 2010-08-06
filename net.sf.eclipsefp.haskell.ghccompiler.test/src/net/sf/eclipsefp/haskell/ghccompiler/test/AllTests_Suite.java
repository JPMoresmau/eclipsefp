package net.sf.eclipsefp.haskell.ghccompiler.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.ghccompiler.test.core.GhcParameterTest;
import net.sf.eclipsefp.haskell.ghccompiler.test.core.UtilTest;


public class AllTests_Suite {

  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTestSuite( GhcParameterTest.class );
    suite.addTestSuite( UtilTest.class );
    return suite;
  }
}
