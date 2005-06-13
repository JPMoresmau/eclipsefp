// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import junit.framework.TestCase;
import junit.framework.TestSuite;

/** <p>collects all test cases in this plugin.</p>
  *
  * @author Leif Frenzel
  */
public class AllTests_PDESuite extends TestCase {

  public static TestSuite suite() {
    TestSuite result = new TestSuite();
    
    result.addTestSuite( CompilationUnit_PDETest.class );
    result.addTestSuite( HaskellParser_PDETest.class );
    
    return result;
  }
}
