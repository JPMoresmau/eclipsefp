package net.sf.eclipsefp.haskell.core.jparser.test;

import antlr.Token;
import junit.framework.TestCase;

public class TokenStreamTestCase extends TestCase {

	protected void assertTokenType(int expectedType, Token token) {
		assertEquals(expectedType, token.getType());
	}

	protected void assertToken(int expectedType, String expectedText, Token token) {
		assertEquals(expectedType, token.getType());
		assertEquals(expectedText, token.getText());
	}
	
}
