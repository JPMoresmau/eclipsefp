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
	
	// The sample for these tests was taken from then Haskell Report
	// and is available at
	// http://www.haskell.org/onlinereport/lexemes.html#layout-before

	public void testFormat() throws TokenStreamException {
		final String inStr = "module Simple where\n" +
        					 "data Stack = Empty\n";
		
		final TokenStream lexer = new HaskellLexer(new StringReader(inStr));

		final TokenStream formatter = new HaskellFormatter(lexer);
		
		Token t = formatter.nextToken();
		assertEquals(HaskellLexerTokenTypes.MODULE, t.getType());
		assertEquals("module", t.getText());
		
		t = formatter.nextToken(); //Simple
		t = formatter.nextToken(); //where
		t = formatter.nextToken(); // {
		assertEquals(HaskellLexerTokenTypes.LEFT_CURLY, t.getType());
		
		t = formatter.nextToken(); //data
		t = formatter.nextToken(); //Stack
		t = formatter.nextToken(); //=
		t = formatter.nextToken(); //Empty

		t = formatter.nextToken(); // }
		assertEquals(HaskellLexerTokenTypes.RIGHT_CURLY, t.getType());
	}
	
	public void testFormatWhere() {
		final String inStr = "module AStack( Stack, push, pop, top, size ) where\n" +
				             "data Stack a = Empty\n" +
				             "             | MkStack a (Stack a)";
		final String outStr = "module AStack( Stack, push, pop, top, size ) where\n" +
                              "{data Stack a = Empty\n" +
                              "              | MkStack a (Stack a)}";
		
		HaskellFormattedReader formatter = new HaskellFormattedReader(
				                               new StringReader(inStr));

		assertOutput(outStr, formatter);
	}
	
	private static void assertOutput(final String expectedOutput, final Reader producer) {
		// setup the tokenizer to break the expectedOutput in lines
		StringTokenizer expected = new StringTokenizer(expectedOutput, "\n\r");
		BufferedReader lineReader = new BufferedReader(producer);
		try {
			int lineCounter = 0;
			while (expected.hasMoreTokens()) {
				lineCounter++;
				final String expectedLine = expected.nextToken();
				final String actualLine = lineReader.readLine();

				if (actualLine == null) {
					throw new AssertionFailedError(
							"Actual output is longer shorter than expected");
				}

				if (!expectedLine.equals(actualLine)) {
					throw new AssertionFailedError("Error at line "
							+ lineCounter + ". Expected '" + expectedLine
							+ "' but read '" + actualLine + "'");
				}
			}

			// maybe the actual output is longer than the expected...
			if (lineReader.readLine() != null) {
				throw new AssertionFailedError(
						"Actual output is longer than expected");
			}
		} catch (IOException e) {
			// this should not happen, since we are using an in-memory
			// reader
			throw new AssertionError(
					"Unexpected I/O error with in-memory reader");
		}
	}
	
}
