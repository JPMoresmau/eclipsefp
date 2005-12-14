package net.sf.eclipsefp.haskell.core.jparser;

import java.util.Stack;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class HaskellFormatter implements TokenStream {

	private static final Token INVALID_TOKEN = new Token(Token.INVALID_TYPE);
	
	private Token fLastToken = INVALID_TOKEN;
	private Stack<Integer> fLayoutContextStack = new Stack<Integer>();
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
			fLayoutContextStack.push(fInput.peekToken().getColumn());
		} else if (isRightCurly(fLastToken)) {
			fOpeningTokenStack.pop();
			fLayoutContextStack.pop();
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
		
		if ( isBlockOpener(fLastToken) &&	!isLeftCurly(peekedToken)) {
			return new Token(HaskellLexerTokenTypes.LEFT_CURLY);
		} else {
			if (isInsideBraces()) {
				if (!(isRightCurly(peekedToken) || isSemicolon(peekedToken))) {
					int openingTokenColumn = fLayoutContextStack.peek();
					if ( peekedToken.getType() == HaskellLexerTokenTypes.EOF ||
					peekedToken.getColumn() < openingTokenColumn) {
					    return new Token(HaskellLexerTokenTypes.RIGHT_CURLY);
					} else if ( peekedToken != fOpeningTokenStack.peek() &&
					peekedToken.getColumn() == openingTokenColumn &&
					!isSemicolon(fLastToken)) {
						return new Token(HaskellLexerTokenTypes.SEMICOLON);
					}
				}
			}
			
			return fInput.nextToken();
		}
	}

	private boolean isBlockOpener(Token token) {
		return isWhere(token) || isLet(token) || isDo(token) || isOf(token);
	}

	private boolean isLet(Token token) {
		return token.getType() == HaskellLexerTokenTypes.LET;
	}

	private boolean isDo(Token token) {
		return token.getType() == HaskellLexerTokenTypes.DO;
	}

	private boolean isOf(Token token) {
		return token.getType() == HaskellLexerTokenTypes.OF;
	}

	private boolean isWhere(Token token) {
		return token.getType() == HaskellLexerTokenTypes.WHERE;
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

	private boolean isLeftCurly(Token theToken) {
		return theToken.getType() == HaskellLexerTokenTypes.LEFT_CURLY;
	}

	private boolean isInsideBraces() {
		return !fOpeningTokenStack.empty();
	}

}
