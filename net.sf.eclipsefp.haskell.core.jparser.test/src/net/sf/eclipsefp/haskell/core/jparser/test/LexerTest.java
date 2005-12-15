package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import antlr.Token;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;

import junit.framework.TestCase;

public class LexerTest extends TestCase implements HaskellLexerTokenTypes {
	
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
		
		assertEquals(MODULE, t.getType());
		assertEquals("module", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Simple", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(WHERE, t.getType());
		assertEquals("where", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(NEWLINE, t.getType());
		assertEquals("\n", t.getText());

		t = fLexer.nextToken();
		assertEquals(DATA, t.getType());
		assertEquals("data", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Underlined_stack", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(EQUALS, t.getType());
		assertEquals("=", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Empty", t.getText());

		t = fLexer.nextToken();
		assertEquals(NEWLINE, t.getType());
		assertEquals("\n", t.getText());

		t = fLexer.nextToken();
		assertEquals(EOF, t.getType());
	}
	
	public void testRecognizeLet() throws TokenStreamException {
		fLexer = createLexer("let in");
		assertEquals(LET, fLexer.nextToken().getType());
	}
	
	public void testCommonPrefixes() throws TokenStreamException {
		fLexer = createLexer("main whomp modula whery");
		
		Token t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("main", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("whomp", t.getText());

		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("modula", t.getText());

		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("whery", t.getText());
	}
	
	public void testKeyworkPreffixInsideIdentifier() throws TokenStreamException {
		fLexer = createLexer("Pwho imodule");
		
		Token t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Pwho", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("imodule", t.getText());
	}
	
	public void testPosition() throws TokenStreamException {
		Token t = fLexer.nextToken(); //module
		assertEquals(0, t.getColumn());
		assertEquals(0, t.getLine());
		
		t = fLexer.nextToken(); // Simple
		assertEquals(7, t.getColumn());
		assertEquals(0, t.getLine());
		
		t = fLexer.nextToken(); // where
		assertEquals(0, t.getLine());
	
		t = fLexer.nextToken(); // \n

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
		assertEquals(COMMENT, t.getType());
		assertEquals("--this is the main module for the app\n", t.getText());
		
		t = fLexer.nextToken(); //module
		assertEquals(0, t.getColumn());
		assertEquals(1, t.getLine());
		
		fLexer.skipTokens(3); //Main where \n
		
		t = fLexer.nextToken();
		assertEquals(COMMENT, t.getType());
		assertEquals("{- We actually need to import those\n" +
	                 "   modules here for using the network\n" +
	                 "   connection capabilities -}",
	                 t.getText());
		
		fLexer.skipTokens(1); // \n

		t = fLexer.nextToken(); //import
		assertEquals(5, t.getLine());
		
		fLexer.skipTokens(5); //Network \n \n main = 
		
		t = fLexer.nextToken(); // {- block comment inside -}
		assertEquals(COMMENT, t.getType());
		assertEquals("{- block comment inside -}", t.getText());
		
		t = fLexer.nextToken(); //putStr
		assertEquals("putStr", t.getText());
		assertEquals(34, t.getColumn());
	}
	
	private TestTokenStream createLexer(String input) {
		return new TestTokenStream(new HaskellLexer(new StringReader(input)));
	}
	
	//TODO maybe we need to recognize more comment formats. the report
	//specifies a return char as the end of a line comment too
	
	//TODO a string literal my span multiple lines

	//TODO scan literate haskell (maybe this doesn't even mess with the lexer)
	//take a look at the Language.Haskell.Parser impl
}
