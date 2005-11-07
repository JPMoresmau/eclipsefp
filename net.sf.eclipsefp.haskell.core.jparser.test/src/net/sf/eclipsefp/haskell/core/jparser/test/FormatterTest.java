package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellFormatter;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;

import junit.framework.TestCase;

public class FormatterTest extends TestCase implements HaskellLexerTokenTypes {
	
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
		assertEquals(MODULE, t.getType());
		assertEquals("module", t.getText());
		
		t = formatter.nextToken(); //Simple
		t = formatter.nextToken(); //where
		t = formatter.nextToken(); // {
		assertEquals(LEFT_CURLY, t.getType());
		
		t = formatter.nextToken(); //data
		assertEquals("data", t.getText());
		
		t = formatter.nextToken(); //Stack
		t = formatter.nextToken(); //=
		t = formatter.nextToken(); //Empty

		t = formatter.nextToken(); // }
		assertEquals(RIGHT_CURLY, t.getType());
		
		t = formatter.nextToken(); // EOF

		assertEquals(EOF, t.getType());
	}
	
	public void testNestedWhere() throws TokenStreamException {
		final String inStr = "module Simple where\n" +
		                     "    id x = a where a = x\n";

		final TokenStream formatter = createFormatter(inStr);
		
		//consume 'module Simple where'
		consumeTokens(formatter, 3);
		assertEquals(LEFT_CURLY, formatter.nextToken().getType());

		//consume 'id x = a where'
		consumeTokens(formatter, 5);
		assertEquals(LEFT_CURLY, formatter.nextToken().getType());
		
		//consume 'a = x'
		consumeTokens(formatter, 3);
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
	}
	
	public void testPlaceSemicolon() throws TokenStreamException {
		final String inStr = "module Simple where\n" +
                             "    fat 0 = 1\n" +
                             "    fat x = x * fat (x - 1)\n" +
                             "    id x = x";
		final TokenStream formatter = createFormatter(inStr);
		
		//consume 'module Simple where { fat 0 = 1'
		consumeTokens(formatter, 8);
		assertEquals(SEMICOLON, formatter.nextToken().getType());
		assertEquals("fat", formatter.nextToken().getText());
		
		//consume 'x = x * fat ( x - 1 )'		
		consumeTokens(formatter, 10);
		assertEquals(SEMICOLON, formatter.nextToken().getType());
		
		//consume 'id x = x'
		consumeTokens(formatter, 4);
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
		
		assertEquals(EOF, formatter.nextToken().getType());
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
