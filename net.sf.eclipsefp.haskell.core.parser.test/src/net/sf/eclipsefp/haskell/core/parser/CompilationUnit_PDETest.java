// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import net.sf.eclipsefp.haskell.core.parser.test.util.Parser_PDETestCase;

import net.sf.eclipsefp.haskell.core.halamo.*;

/** <p>tests for compilation units (src location support).</p>
  *
  * @author Leif Frenzel
  */
public class CompilationUnit_PDETest extends Parser_PDETestCase {

  public void testEmptyCU() throws Exception {
    // an empty compilation unit has an implicit Main module at position 0,0
    // (apart from leading comment lines like in the target file, which makes
    // it in this case position 1,0 )
    ICompilationUnit cu = parseAsFile("-- nothing in here, so no src locations\n");
    ISourceLocation sl1 = cu.getNextLocation( new SourceLocation( 0, 0 ) );
    assertTrue( sl1 != null );
    assertEquals( 1, sl1.getLine() );
    assertEquals( 0, sl1.getColumn() );    
    ISourceLocation sl2 = cu.getNextLocation( new SourceLocation( 3, 3 ) );
    assertTrue( sl2 == null );
  }
  
  public void testSingleSrcLoc() throws Exception {
    final String input = "-- only one src location\n" +
    		             "module Main (main, \n" +
    		             "             getTheAnswer,\n" +
    		             "             module Haskell.Language.Syntax) where";
	ICompilationUnit cu = parseAsFile(input);
    ISourceLocation sl1 = cu.getNextLocation( new SourceLocation( 0, 0 ) );
    assertTrue( sl1 != null );
    assertEquals( 1, sl1.getLine() );
    assertEquals( 0, sl1.getColumn() );
    
    ISourceLocation sl2 = cu.getNextLocation( new SourceLocation( 3, 3 ) );
    assertTrue( sl2 == null );
  }

  public void testTwoSrcLocs() throws Exception {
    final String input = "-- only one src location\n" +
    		             "module Main (main,\n" +
    		             "             getTheAnswer,\n" +
    		             "             module Haskell.Language.Syntax) where\n" +
    		             "\n" +
    		             "import Haskell.Language.Syntax";

	ICompilationUnit cu = parseAsFile(input);
    ISourceLocation sl1 = cu.getNextLocation( new SourceLocation( 0, 0 ) );
    assertTrue( sl1 != null );
    assertEquals( 1, sl1.getLine() );
    assertEquals( 0, sl1.getColumn() );
    
    ISourceLocation sl2 = cu.getNextLocation( new SourceLocation( 3, 3 ) );
    assertTrue( sl2 != null );
    assertEquals( 5, sl2.getLine() );
    assertEquals( 0, sl2.getColumn() );
    
    ISourceLocation sl3 = cu.getNextLocation( new SourceLocation( 6, 3 ) );
    assertTrue( sl3 == null );
  }

  public void testLotsOfLocs() throws Exception {
	final String input = "-- lots of things that have source locations\n" +
			             "module Main (main,\n" +
			             "             getTheAnswer,\n" +
			             "             module Haskell.Language.Syntax) where\n" +
			             "\n" +
			             "import Haskell.Language.Syntax\n" +
			             "import Bla\n" +
			             "import Blubb\n" +
			             "\n" +
			             "data Eq a => Set a =   NilSet\n" +
			             "                     | ConsSet a (Set a)\n" +
			             "\n" +
			             "data Temp = Cold | Hot\n" +
			             "\n" +
			             "idf2, idf3 :: Int\n" +
			             "idf2 = 42\n" +
			             "\n" +
			             "getTheAnswer, getItAgain :: Int -> Int\n" +
			             "getTheAnswer n = 42\n" +
			             "\n" +
			             "class Visible a where\n" +
			             "  toString :: a -> String\n" +
			             "  size :: a -> Int\n" +
			             "\n" +
			             "instance Visible Bla where\n" +
			             "  toString Bla = \"Bla\"\n" +
			             "  size Bla = 42\n" +
			             "\n" +
			             "infix 5 `op1`\n" +
			             "infixr 1 `op2`\n" +
			             "\n" +
			             "infixl 5 `op2`\n" +
			             "\n" +
			             "infix 5 `op1`, `op2`\n" +
			             "infixl 0 `op1`, +, `opx`\n" +
			             "\n" +
			             "newtype Age = Age { unAge :: Int }\n" +
			             "\n" +
			             "type Rec a = [Circ a]\n" +
			             "\n" +
			             "default (Integer, Double)";
    ICompilationUnit cu = parseAsFile(input);
    ISourceLocation sl1 = cu.getNextLocation( new SourceLocation( 0, 0 ) );
    assertEquals( 1, sl1.getLine() );
    assertEquals( 0, sl1.getColumn() );

    ISourceLocation sl2 = cu.getNextLocation( sl1 );
    assertEquals( 5, sl2.getLine() );
    assertEquals( 0, sl2.getColumn() );
    
    ISourceLocation sl3 = cu.getNextLocation( sl2 );
    assertEquals( 6, sl3.getLine() );
    assertEquals( 0, sl3.getColumn() );
    
    ISourceLocation sl4 = cu.getNextLocation( sl3 );
    assertEquals( 7, sl4.getLine() );
    assertEquals( 0, sl4.getColumn() );
    
    ISourceLocation sl5 = cu.getNextLocation( sl4 );
    assertEquals( 9, sl5.getLine() );
    assertEquals( 0, sl5.getColumn() );
    
    ISourceLocation sl6 = cu.getNextLocation( sl5 );
    assertEquals( 12, sl6.getLine() );
    assertEquals( 0, sl6.getColumn() );
    
    ISourceLocation sl7 = cu.getNextLocation( new SourceLocation( 39, 3 )  );
    assertEquals( 40, sl7.getLine() );
    assertEquals( 0, sl7.getColumn() );
    
    ISourceLocation sl8 = cu.getNextLocation( sl7  );
    assertTrue( sl8 == null );
  }
  
}
