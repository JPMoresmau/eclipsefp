package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerExtendedTokenTypes;
import net.sf.eclipsefp.haskell.core.jparser.PreprocessedTokenStream;
import antlr.Token;
import antlr.TokenStreamException;
import junit.framework.TestCase;

public class PreprocessedTokenStreamTest extends TestCase implements HaskellLexerExtendedTokenTypes {
	
	public void testInsertLinebreakToken() throws TokenStreamException {
		final String inStr = "module TokenStreamTest\n" +
				             "    fat 0 = 1";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		// module TokenStreamTest
		stream.skipTokens(2);
		Token t = stream.nextToken();
		
		assertEquals(LINEBREAK, t.getType());
		assertEquals(4, t.getColumn());
	}
	
	public void testInsertBlockOpenToken() throws TokenStreamException {
		final String inStr = "module TokenStreamTest where fat n = case n of 0 -> let x = 1 in do x\n";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		// module TokenStreamTest where
		stream.skipTokens(3);
		Token t = stream.nextToken();
		
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(29, t.getColumn());
		
		// fat n = case n of
		stream.skipTokens(6);
		t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(47, t.getColumn());
		
		// 0 -> let
		stream.skipTokens(3);
		t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(56, t.getColumn());
		
		// x = 1 in do
		stream.skipTokens(5);
		t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(68, t.getColumn());
	}
	
	public void testInsertBlockOpenTokenBeforeLineBreak() throws TokenStreamException {
		final String inStr = "module TokenStreamTest where\n" +
				             "    fat 0 = 1\n";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		// module TokenStreamTest where
		stream.skipTokens(3);
		Token t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(4, t.getColumn());
	}
	
	public void testNoLineBreakAfterBlockOpen() throws TokenStreamException {
		final String inStr = "module TokenStreamTest where\n" +
        					 "    fat 0 = 1\n";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		// module TokenStreamTest where
		stream.skipTokens(3);
		assertEquals(OPENBLOCK, stream.nextToken().getType());
		assertEquals(VARIABLE_ID, stream.nextToken().getType());
	}
	
	public void testUntitledModule() throws TokenStreamException {
		final String inStr = "fat 0 = 1";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		Token t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(0, t.getColumn());
	}
	
	public void testEmptyModule() throws TokenStreamException {
		final String inStr = "module Empty where";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		stream.skipTokens(3);
		Token t = stream.nextToken();
		assertEquals(OPENBLOCK, t.getType());
		assertEquals(-1, t.getColumn());
		assertEquals(EOF, stream.nextToken().getType());
	}

	public void testExtraLinebreak() throws TokenStreamException {
		final String inStr = "    fat 0 = 1\n" +
			                 "\n" +
					         "    fat n = n * (fat (n - 1))\n";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		// {4} fat 0 = 1
		stream.skipTokens(5);
		Token lineBreak = stream.nextToken();
		assertEquals(LINEBREAK, lineBreak.getType());
		assertEquals(4, lineBreak.getColumn());
		
		assertEquals(VARIABLE_ID, stream.nextToken().getType());
	}
	
	public void testNoLinebreakForEndOfFile() throws TokenStreamException {
		final String inStr = "    fat 0 = 1\n" +
                             "    fat n = n * (fat (n - 1))\n";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		// {4} fat 0 = 1
		// <4> fat n = n * ( fat ( n - 1 ) )
		stream.skipTokens(5);
		stream.skipTokens(14);
		
		assertEquals(EOF, stream.nextToken().getType());
	}
	
	public void testStartWithLinebreaks() throws TokenStreamException {
		final String inStr = "\n" +
				             "\n" +
				             "  main = putStr \"Hello, world\"";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		// {2} main
		Token token = stream.nextToken();
		assertEquals(OPENBLOCK, token.getType());
		assertEquals(2, token.getColumn());
		
		token = stream.nextToken();
		assertEquals(VARIABLE_ID, token.getType());
		assertEquals("main", token.getText());
	}
	
	public void testModuleAfterLinebreak() throws TokenStreamException {
		final String inStr = "\n" +
				             "   module ParserTest where {}";
		final TestTokenStream stream = createPreprocessor(inStr);
		
		Token token = stream.nextToken();
		assertEquals(MODULE, token.getType());
		assertEquals(3, token.getColumn());
	}

	private TestTokenStream createPreprocessor(final String inStr) {
		return new TestTokenStream(
				new PreprocessedTokenStream(
				 new HaskellLexer(new StringReader(inStr))));
	}
	
}
