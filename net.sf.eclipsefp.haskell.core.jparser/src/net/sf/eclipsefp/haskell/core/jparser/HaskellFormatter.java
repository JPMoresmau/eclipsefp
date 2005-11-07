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
		if ( fLastToken.getType() == HaskellLexerTokenTypes.WHERE &&
		fInput.peekToken().getType() != HaskellLexerTokenTypes.LEFT_CURLY) {
			fLastToken = new Token(HaskellLexerTokenTypes.LEFT_CURLY);
			fOpeningTokenStack.push(fInput.peekToken());
		} else {
			Token nextToken = fInput.peekToken();
			if (isInsideBraces()) {
				Token openingToken = fOpeningTokenStack.peek();
				if ( nextToken.getType() == HaskellLexerTokenTypes.EOF ||
				nextToken.getColumn() < openingToken.getColumn()) {
					fOpeningTokenStack.pop();
					return new Token(HaskellLexerTokenTypes.RIGHT_CURLY);
				} else if ( nextToken != openingToken &&
				fLastToken.getType() != HaskellLexerTokenTypes.SEMICOLON &&
				nextToken.getColumn() == openingToken.getColumn()) {
					return fLastToken = new Token(HaskellLexerTokenTypes.SEMICOLON);
				}
			}
			
			fLastToken = fInput.nextToken();
		}
		return fLastToken;
	}

	private boolean isInsideBraces() {
		return !fOpeningTokenStack.empty();
	}

}
