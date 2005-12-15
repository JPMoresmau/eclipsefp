package net.sf.eclipsefp.haskell.core.jparser.test;

import java.util.List;
import java.util.Vector;

import net.sf.eclipsefp.haskell.core.jparser.LookaheadTokenStream;

import antlr.Token;
import antlr.TokenStreamException;

import junit.framework.TestCase;

public class LookaheadTokenStreamTest extends TestCase {
	
	private LookaheadTokenStream fStream;

	protected void setUp() {
		List<Token> tokens = new Vector<Token>();
		
		tokens.add(new Token(1));
		tokens.add(new Token(2));
		tokens.add(new Token(3));
		tokens.add(new Token(4));
		
		fStream = new LookaheadTokenStream(new ListTokenStream(tokens));
	}
	
	public void testStartWithLookahead() throws TokenStreamException {
		Token peekedToken = fStream.peekToken();
		assertEquals(1, peekedToken.getType());
		
		Token aToken = fStream.nextToken();
		assertEquals(1, aToken.getType());
		assertSame(peekedToken, aToken);
		
		aToken = fStream.nextToken();
		assertEquals(2, aToken.getType());
	}

	public void testRepeatedLookahead() throws TokenStreamException {
		
		Token aToken = fStream.nextToken();
		assertEquals(1, aToken.getType());
		
		Token peekedToken = fStream.peekToken();
		assertEquals(2, peekedToken.getType());
		
		peekedToken = fStream.peekToken();
		assertEquals(2, peekedToken.getType());
		
		aToken = fStream.nextToken();
		assertEquals(2, aToken.getType());
		
		aToken = fStream.nextToken();
		assertEquals(3, aToken.getType());
	}
	
	public void testLookaheadOnLastElement() throws TokenStreamException {
		for(int i = 0; i < 3; ++i) {
			fStream.nextToken();
		}
		
		Token peekedToken = fStream.peekToken();
		Token aToken = fStream.nextToken();
		
		assertSame(peekedToken, aToken);
		assertEquals(4, aToken.getType());
	}
	
	public void testLookaheadAfterFirst() throws TokenStreamException {
		Token fstToken = fStream.peekToken(1);
		Token sndToken = fStream.peekToken(2);
		
		assertEquals(1, fstToken.getType());
		assertEquals(2, sndToken.getType());
		
		Token aToken = fStream.nextToken();
		assertSame(fstToken, aToken);
		
		aToken = fStream.nextToken();
		assertSame(sndToken, aToken);
	}
	
}
