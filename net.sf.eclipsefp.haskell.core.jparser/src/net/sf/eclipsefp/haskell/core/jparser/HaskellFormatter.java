package net.sf.eclipsefp.haskell.core.jparser;

import java.util.Stack;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class HaskellFormatter implements TokenStream {

	private static final Token INVALID_TOKEN = new Token(Token.INVALID_TYPE);
	
	private Token fLastToken = INVALID_TOKEN;
	private Stack<Token> fOpeningTokenStack = new Stack<Token>();
	private LookaheadTokenStream fInput;
	
	public HaskellFormatter(TokenStream in) {
		fInput = new LookaheadTokenStream(in);
	}

	public Token nextToken() throws TokenStreamException {
		fLastToken = calculateNextToken();

		if (isLeftCurly(fLastToken)) {
			fOpeningTokenStack.push(fInput.peekToken());
		} else if (isRightCurly(fLastToken)) {
			fOpeningTokenStack.pop();
		}
		
		return fLastToken;
	}

	private Token calculateNextToken() throws TokenStreamException {
		if ( isWhere(fLastToken) &&	!isLeftCurly(fInput.peekToken())) {
			return new Token(HaskellLexerTokenTypes.LEFT_CURLY);
		} else {
			Token nextToken = fInput.peekToken();
			if (isInsideBraces()) {
				Token openingToken = fOpeningTokenStack.peek();
				if (!(isRightCurly(nextToken) || isSemicolon(nextToken))) {
					if ( nextToken.getType() == HaskellLexerTokenTypes.EOF ||
					nextToken.getColumn() < openingToken.getColumn()) {
					    return new Token(HaskellLexerTokenTypes.RIGHT_CURLY);
					} else if ( nextToken != openingToken &&
					nextToken.getColumn() == openingToken.getColumn() &&
					!isSemicolon(fLastToken)) {
						return new Token(HaskellLexerTokenTypes.SEMICOLON);
					}
				}
			}
			
			return fInput.nextToken();
		}
	}

	private boolean isSemicolon(Token token) {
		return token.getType() == HaskellLexerTokenTypes.SEMICOLON;
	}

	private boolean isRightCurly(Token theToken) {
		return theToken.getType() == HaskellLexerTokenTypes.RIGHT_CURLY;
	}

	private boolean isWhere(Token token) {
		return token.getType() == HaskellLexerTokenTypes.WHERE;
	}

	private boolean isLeftCurly(Token theToken) {
		return theToken.getType() == HaskellLexerTokenTypes.LEFT_CURLY;
	}

	private boolean isInsideBraces() {
		return !fOpeningTokenStack.empty();
	}

}
