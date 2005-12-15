package net.sf.eclipsefp.haskell.core.jparser;

import java.util.Stack;

import com.sun.org.apache.bcel.internal.generic.FCONST;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class HaskellFormatter implements TokenStream {

	private static final Token INVALID_TOKEN = new Token(Token.INVALID_TYPE);
	
	private Token fLastToken = INVALID_TOKEN;
	private Stack<Integer> fLayoutContextStack = new Stack<Integer>();
	private Stack<Token> fOpeningTokenStack = new Stack<Token>();
	private Stack<Token> fInsertedTokens = new Stack<Token>();
	
	private LookaheadTokenStream fInput;

	private boolean fIsFirstCall = true;

	public HaskellFormatter(TokenStream in) {
		fInput = new LookaheadTokenStream(in);
	}

	public Token nextToken() throws TokenStreamException {
		insertTokensAsNeeded();
		
		if (fInsertedTokens.isEmpty()) {
			return fInput.nextToken();
		} else {
			return fInsertedTokens.pop();
		}
	}

	private void insertTokensAsNeeded() throws TokenStreamException {
		if (!fInsertedTokens.isEmpty())
			return;
		
		if (fIsFirstCall && !isModule(fInput.peekToken()) && !isLeftCurly(fInput.peekToken())) {
			Token referenceToken = fInput.nextToken();
			fInsertedTokens.push(referenceToken);
			fInsertedTokens.push(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
			fLayoutContextStack.push(referenceToken.getColumn());
		}
		
		fIsFirstCall = false;
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
