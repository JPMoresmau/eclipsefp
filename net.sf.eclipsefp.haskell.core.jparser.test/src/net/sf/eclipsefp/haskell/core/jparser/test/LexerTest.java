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

	private HaskellLexer fLexer;

	protected void setUp() {
		final String inStr = "module Simple where\n" +
				             "data Underlined_stack = Empty\n";
		
		fLexer = new HaskellLexer(new StringReader(inStr));
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
	
	public void testCommonPrefixes() throws TokenStreamException {
		fLexer = new HaskellLexer(new StringReader("main whomp modula whery"));
		
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

	public void testPosition() throws TokenStreamException {
		Token t = fLexer.nextToken(); //module
		assertEquals(1, t.getColumn());
		assertEquals(1, t.getLine());
		
		t = fLexer.nextToken();
		assertEquals(8, t.getColumn());
		assertEquals(1, t.getLine());
		
		t = fLexer.nextToken();
		assertEquals(1, t.getLine());
	
		t = fLexer.nextToken();
		assertEquals(1, t.getColumn());
		assertEquals(2, t.getLine());

		t = fLexer.nextToken();
		assertEquals(6, t.getColumn());
		assertEquals(2, t.getLine());
	}
	
	//TODO scan literate haskell
}
