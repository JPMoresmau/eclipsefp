package net.sf.eclipsefp.haskell.ghccompiler.test;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.ghccompiler.test.core.GhcOutputParserTest;
import net.sf.eclipsefp.haskell.ghccompiler.test.core.ProcessRunner_Test;

public class AllTests_Suite {

  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTestSuite( GhcOutputParserTest.class );
    suite.addTestSuite( ProcessRunner_Test.class );
    return suite;
  }
}
