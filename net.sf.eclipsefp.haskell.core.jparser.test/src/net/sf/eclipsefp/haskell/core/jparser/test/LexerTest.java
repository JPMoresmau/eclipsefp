package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import antlr.Token;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;

import junit.framework.TestCase;

public class LexerTest extends TestCase {
	
	// The sample for these tests was taken from then Haskell Report
	// and is available at
	// http://www.haskell.org/onlinereport/lexemes.html#layout-before

	private TestTokenStream fLexer;

	protected void setUp() {
		final String inStr = "module Simple where\n" +
				             "data Underlined_stack = Empty\n";
		
		fLexer = new TestTokenStream(new HaskellLexer(new StringReader(inStr)));
	}

	public void testRecognition() throws TokenStreamException {
		Token t = fLexer.nextToken();
		
		assertEquals(HaskellLexerTokenTypes.MODULE, t.getType());
		assertEquals("module", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.CONSTRUCTOR_ID, t.getType());
		assertEquals("Simple", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.WHERE, t.getType());
		assertEquals("where", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.VARIABLE_ID, t.getType());
		assertEquals("data", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.CONSTRUCTOR_ID, t.getType());
		assertEquals("Underlined_stack", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.SYMBOL, t.getType());
		assertEquals("=", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.CONSTRUCTOR_ID, t.getType());
		assertEquals("Empty", t.getText());

		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.EOF, t.getType());
	}
	
	private TestTokenStream createLexer(String input) {
		return new TestTokenStream(new HaskellLexer(new StringReader(input)));
	}
	
	public void testCommonPrefixes() throws TokenStreamException {
		fLexer = createLexer("main whomp modula whery");
		
		Token t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.VARIABLE_ID, t.getType());
		assertEquals("main", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.VARIABLE_ID, t.getType());
		assertEquals("whomp", t.getText());

		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.VARIABLE_ID, t.getType());
		assertEquals("modula", t.getText());

		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.VARIABLE_ID, t.getType());
		assertEquals("whery", t.getText());
	}
	
	public void testKeyworkPreffixInsideIdentifier() throws TokenStreamException {
		fLexer = createLexer("Pwho imodule");
		
		Token t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.CONSTRUCTOR_ID, t.getType());
		assertEquals("Pwho", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.VARIABLE_ID, t.getType());
		assertEquals("imodule", t.getText());
	}
	
	public void testPosition() throws TokenStreamException {
		Token t = fLexer.nextToken(); //module
		assertEquals(0, t.getColumn());
		assertEquals(0, t.getLine());
		
		t = fLexer.nextToken();
		assertEquals(7, t.getColumn());
		assertEquals(0, t.getLine());
		
		t = fLexer.nextToken();
		assertEquals(0, t.getLine());
	
		t = fLexer.nextToken();
		assertEquals(0, t.getColumn());
		assertEquals(1, t.getLine());

		t = fLexer.nextToken();
		assertEquals(5, t.getColumn());
		assertEquals(1, t.getLine());
	}
	
	public void testCodeWithComments() throws TokenStreamException {
		fLexer = createLexer("--this is the main module for the app\n" +
				             "module Main where\n" +
				             "{- We actually need to import those\n" +
				             "   modules here for using the network\n" +
				             "   connection capabilities -}\n" +
				             "import Network\n" +
				             "\n" +
				             "main = {- block comment inside -} putStr 'hello'\n"
				             );
		Token t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.COMMENT, t.getType());
		assertEquals("--this is the main module for the app\n", t.getText());
		
		t = fLexer.nextToken(); //module
		assertEquals(0, t.getColumn());
		assertEquals(1, t.getLine());
		
		fLexer.skipTokens(2); //Main where
		
		t = fLexer.nextToken();
		assertEquals(HaskellLexerTokenTypes.COMMENT, t.getType());
		assertEquals("{- We actually need to import those\n" +
	                 "   modules here for using the network\n" +
	                 "   connection capabilities -}",
	                 t.getText());
		
		t = fLexer.nextToken(); //import
		assertEquals(5, t.getLine());
		
		fLexer.skipTokens(3); //Network main = 
		
		t = fLexer.nextToken(); // {- block comment inside -}
		assertEquals(HaskellLexerTokenTypes.COMMENT, t.getType());
		assertEquals("{- block comment inside -}", t.getText());
		
		t = fLexer.nextToken(); //putStr
		assertEquals("putStr", t.getText());
		assertEquals(34, t.getColumn());
	}
	
	//TODO maybe we need to recognize more comment formats. the report
	//specifies a return char as the end of a line comment too 

	//TODO scan literate haskell
}
