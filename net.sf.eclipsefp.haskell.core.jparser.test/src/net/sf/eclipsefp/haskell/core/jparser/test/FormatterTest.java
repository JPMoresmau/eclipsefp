package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.StringTokenizer;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellFormattedReader;
import net.sf.eclipsefp.haskell.core.jparser.HaskellFormatter;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

public class FormatterTest extends TestCase {
	
	private static TokenStream createFormatter(final String input) {
		final TokenStream lexer = new HaskellLexer(new StringReader(input));

		return new HaskellFormatter(lexer);
	}
	
	// The sample for these tests was taken from then Haskell Report
	// and is available at
	// http://www.haskell.org/onlinereport/lexemes.html#layout-before

	public void testSimpleInput() throws TokenStreamException {
		final String inStr = "module Simple where\n" +
        					 "data Stack = Empty\n";
		
		final TokenStream formatter = createFormatter(inStr);
		
		Token t = formatter.nextToken();
		assertEquals(HaskellLexerTokenTypes.MODULE, t.getType());
		assertEquals("module", t.getText());
		
		t = formatter.nextToken(); //Simple
		t = formatter.nextToken(); //where
		t = formatter.nextToken(); // {
		assertEquals(HaskellLexerTokenTypes.LEFT_CURLY, t.getType());
		
		t = formatter.nextToken(); //data
		assertEquals("data", t.getText());
		
		t = formatter.nextToken(); //Stack
		t = formatter.nextToken(); //=
		t = formatter.nextToken(); //Empty

		t = formatter.nextToken(); // }
		assertEquals(HaskellLexerTokenTypes.RIGHT_CURLY, t.getType());
		
		t = formatter.nextToken(); // EOF

		assertEquals(HaskellLexerTokenTypes.EOF, t.getType());
	}

	/**
	 *  Consume <code>num</code> tokens from <code>stream</code>.
	 *  
	 * @param stream The token stream to be consumed
	 * @param num    The number of tokens to consume
	 * @throws TokenStreamException
	 */
	private void consumeTokens(final TokenStream stream, int num) throws TokenStreamException {
		for(int i = 0; i < num; ++i) {
			stream.nextToken(); 
		}
	}
	
	
}
