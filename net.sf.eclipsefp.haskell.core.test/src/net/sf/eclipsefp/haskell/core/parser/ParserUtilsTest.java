package net.sf.eclipsefp.haskell.core.parser;

import junit.framework.TestCase;


/**
 *
 * @author JP Moresmau
 *
 */
public class ParserUtilsTest extends TestCase {


  public ParserUtilsTest( final String name ) {
    super( name );
  }

  public void testIdentifier(){
    String line="d2_dList::[TestD]";
    assertEquals("",ParserUtils.getHaskellWord( line, -1 ));
    assertEquals("d2_dList",ParserUtils.getHaskellWord( line, 0 ));
    assertEquals("d2_dList",ParserUtils.getHaskellWord( line, 7 ));
    assertEquals("",ParserUtils.getHaskellWord( line, 8 ));
    assertEquals("TestD",ParserUtils.getHaskellWord( line, 11 ));
    assertEquals("TestD",ParserUtils.getHaskellWord( line, 12 ));
    assertEquals("TestD",ParserUtils.getHaskellWord( line, 15 ));
    assertEquals("",ParserUtils.getHaskellWord( line, 16 ));
    assertEquals("",ParserUtils.getHaskellWord( line, line.length()-1 ));
    assertEquals("",ParserUtils.getHaskellWord( line, line.length()));
    assertEquals("",ParserUtils.getHaskellWord( line, 255));
  }
}
