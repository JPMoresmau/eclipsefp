package net.sf.eclipsefp.haskell.core.util;

import java.util.regex.Matcher;
import junit.framework.TestCase;


public class GHCiSyntaxTest extends TestCase {

  public GHCiSyntaxTest( final String name ) {
    super( name );
  }

  public void testBreakpointPattern(){
    String s="Breakpoint 0 activated at D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\Haskell0\\src\\Main.hs:9:4-14"+ResourceUtil.NL+"*Main> ";
    Matcher m=GHCiSyntax.BREAKPOINT_SET_PATTERN.matcher( s );
    assertTrue(m.find());
    assertEquals("0",m.group( 1 ));
    assertEquals("D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\Haskell0\\src\\Main.hs:9:4-14",m.group( 2 ));
  }

  public void testBreakpointLocationPattern(){
    String s="D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\Haskell0\\src\\Main.hs:9:4-14";
    //Pattern p=Pattern.compile( "([^\\:]+)");
    Matcher m=GHCiSyntax.BREAKPOINT_LOCATION_PATTERN.matcher( s );
    assertTrue(m.matches());
    assertEquals("D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\Haskell0\\src\\Main.hs",m.group( 1 ));
    assertEquals("9",m.group( 2 ));
    assertEquals("4",m.group( 3 ));
    assertEquals("14",m.group( 4 ));
  }

  public void testBreakpointLocationMultilinePattern(){
    String s="D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\Haskell0\\src\\Test.hs:(29,0)-(35,13)";
    //Pattern p=Pattern.compile( "([^\\:]+)");
    Matcher m=GHCiSyntax.BREAKPOINT_LOCATIONMULTILINE_PATTERN.matcher( s );
    assertTrue(m.matches());
    assertEquals("D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\Haskell0\\src\\Test.hs",m.group( 1 ));
    assertEquals("29",m.group( 2 ));
    assertEquals("0",m.group( 3 ));
    assertEquals("35",m.group( 4 ));
    assertEquals("13",m.group( 5 ));
  }

  public void testBindingPattern(){
    String s="_result :: IO String = _";
    Matcher m=GHCiSyntax.BINDING_PATTERN.matcher( s );
    assertTrue(m.matches());
    assertEquals("_result",m.group(1));
    assertEquals("IO String",m.group(2));
    assertEquals("_",m.group(4));

    s="_result :: IO String";
    m=GHCiSyntax.BINDING_PATTERN.matcher( s );
    assertTrue(m.matches());
    assertEquals("_result",m.group(1));
    assertEquals("IO String",m.group(2));
    assertNull( m.group(3));
    assertNull( m.group(4));
  }
}
