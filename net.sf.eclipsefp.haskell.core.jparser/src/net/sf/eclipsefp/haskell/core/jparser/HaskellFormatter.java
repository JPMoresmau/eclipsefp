package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class HaskellFormatter implements TokenStream {

	private static final Token INVALID_TOKEN = new Token(Token.INVALID_TYPE);
	
	private Token lastToken = INVALID_TOKEN;
	private Token openingToken = INVALID_TOKEN;
	private LookaheadTokenStream fInput;
	
	public HaskellFormatter(TokenStream in) {
		fInput = new LookaheadTokenStream(in);
	}

	public Token nextToken() throws TokenStreamException {
		if ( lastToken.getType() == HaskellLexerTokenTypes.WHERE ) {
			lastToken = new Token(HaskellLexerTokenTypes.LEFT_CURLY);
			openingToken = fInput.peekToken();
		} else {
			Token nextToken = fInput.peekToken();
			if (isInsideBraces()) {
				if ( nextToken.getType() == HaskellLexerTokenTypes.EOF ||
				nextToken.getColumn() < openingToken.getColumn()) {
					openingToken = null;
					return new Token(HaskellLexerTokenTypes.RIGHT_CURLY);
				}
			}
			
			lastToken = fInput.nextToken();
		}
		return lastToken;
	}

	private boolean isInsideBraces() {
		return openingToken != null;
	}

}
