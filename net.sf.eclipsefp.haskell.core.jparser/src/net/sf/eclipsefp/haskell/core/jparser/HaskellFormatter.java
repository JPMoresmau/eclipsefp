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

	private boolean fIsFirstCall = true;
	
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
		final Token peekedToken = fInput.peekToken();
		if (fIsFirstCall) {
			fIsFirstCall = false;
			if (!isModule(peekedToken) && !isLeftCurly(peekedToken)) {
			    return new Token(HaskellLexerTokenTypes.LEFT_CURLY);
			}
		}
		
		if ( isWhere(fLastToken) &&	!isLeftCurly(peekedToken)) {
			return new Token(HaskellLexerTokenTypes.LEFT_CURLY);
		} else {
			if (isInsideBraces()) {
				Token openingToken = fOpeningTokenStack.peek();
				if (!(isRightCurly(peekedToken) || isSemicolon(peekedToken))) {
					if ( peekedToken.getType() == HaskellLexerTokenTypes.EOF ||
					peekedToken.getColumn() < openingToken.getColumn()) {
					    return new Token(HaskellLexerTokenTypes.RIGHT_CURLY);
					} else if ( peekedToken != openingToken &&
					peekedToken.getColumn() == openingToken.getColumn() &&
					!isSemicolon(fLastToken)) {
						return new Token(HaskellLexerTokenTypes.SEMICOLON);
					}
				}
			}
			
			return fInput.nextToken();
		}
	}

	private boolean isModule(Token token) {
		return token.getType() == HaskellLexerTokenTypes.MODULE;
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
