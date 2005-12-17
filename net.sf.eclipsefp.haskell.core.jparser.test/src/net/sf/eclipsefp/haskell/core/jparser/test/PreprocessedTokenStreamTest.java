package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerExtendedTokenTypes;
import net.sf.eclipsefp.haskell.core.jparser.PreprocessedTokenStream;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;
import junit.framework.TestCase;

public class PreprocessedTokenStreamTest extends TestCase implements HaskellLexerExtendedTokenTypes {
	
	public void testInsertLinebreakToken() throws TokenStreamException {
		final String inStr = "module TokenStreamTest\n" +
				             "    fat 0 = 1";
		final PreprocessedTokenStream stream = new PreprocessedTokenStream(new HaskellLexer(new StringReader(inStr)));
		
		// module TokenStreamTest
		skipTokens(stream, 2);
		Token t = stream.nextToken();
		
		assertEquals(LINEBREAK, t.getType());
		assertEquals(4, t.getColumn());
	}
	
	public void testInsertBlockOpenToken() throws TokenStreamException {
		final String inStr = "module TokenStreamTest where fat n = case n of 0 -> let x = 1 in do x\n";
		final PreprocessedTokenStream stream = new PreprocessedTokenStream(new HaskellLexer(new StringReader(inStr)));
		
		// module TokenStreamTest where
		skipTokens(stream, 3);
		Token t = stream.nextToken();
		
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(29, t.getColumn());
		
		// fat n = case n of
		skipTokens(stream, 6);
		t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(47, t.getColumn());
		
		// 0 - > let
		skipTokens(stream, 4);
		t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(56, t.getColumn());
		
		// x = 1 in do
		skipTokens(stream, 5);
		t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(68, t.getColumn());
	}
	
	public void testInsertBlockOpenTokenBeforeLineBreak() throws TokenStreamException {
		final String inStr = "module TokenStreamTest where\n" +
				             "    fat 0 = 1\n";
		final PreprocessedTokenStream stream = new PreprocessedTokenStream(new HaskellLexer(new StringReader(inStr)));
		
		// module TokenStreamTest where
		skipTokens(stream, 3);
		Token t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(4, t.getColumn());
	}
	
	public void testNoLineBreakAfterBlockOpen() throws TokenStreamException {
		final String inStr = "module TokenStreamTest where\n" +
        					 "    fat 0 = 1\n";
		final PreprocessedTokenStream stream = new PreprocessedTokenStream(new HaskellLexer(new StringReader(inStr)));
		
		// module TokenStreamTest where
		skipTokens(stream, 3);
		assertEquals(OPENBLOCK, stream.nextToken().getType());
		assertEquals(VARIABLE_ID, stream.nextToken().getType());
	}
	
	public void testUntitledModule() throws TokenStreamException {
		final String inStr = "fat 0 = 1";
		final PreprocessedTokenStream stream = new PreprocessedTokenStream(new HaskellLexer(new StringReader(inStr)));
		
		Token t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(0, t.getColumn());
	}
	
	public void testEmptyModule() throws TokenStreamException {
		final String inStr = "module Empty where";
		final PreprocessedTokenStream stream = new PreprocessedTokenStream(new HaskellLexer(new StringReader(inStr)));
		
		skipTokens(stream, 3);
		Token t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(-1, t.getColumn());
		assertEquals(EOF, stream.nextToken().getType());
	}

	private void skipTokens(TokenStream stream, int n) throws TokenStreamException {
		for(int i = 0; i < n; ++i) {
			stream.nextToken();
		}
	}

}
