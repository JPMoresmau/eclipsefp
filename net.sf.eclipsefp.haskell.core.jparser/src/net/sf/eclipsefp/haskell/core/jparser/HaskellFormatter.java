package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class HaskellFormatter implements TokenStream {

	private Stack<Integer> fLayoutContextStack = new Stack<Integer>();
	private Stack<Token> fOpeningTokenStack = new Stack<Token>();
	private Queue<Token> fInsertedTokens = new LinkedList<Token>();
	
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
			return fInsertedTokens.poll();
		}
	}

	private void insertTokensAsNeeded() throws TokenStreamException {
		if (!fInsertedTokens.isEmpty())
			return;
		
		boolean needToOpenBlock = false;
		if (fIsFirstCall && !isModule(fInput.peekToken()) && !isLeftCurly(fInput.peekToken()))
		{
			needToOpenBlock = true;
		} else if (isBlockOpener(fInput.peekToken(1)) && !isLeftCurly(fInput.peekToken(2))) {
			fInsertedTokens.offer(fInput.nextToken());
			needToOpenBlock = true;
		}
		
		if (needToOpenBlock) {
			Token referenceToken = fInput.nextToken();;
			fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
			if (!isEof(referenceToken)) {
				fInsertedTokens.offer(referenceToken);
				fLayoutContextStack.push(referenceToken.getColumn());
			} else {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.RIGHT_CURLY));
			}
		}
		
		fIsFirstCall = false;
	}

	private boolean isEof(Token token) {
		return token.getType() == HaskellLexerTokenTypes.EOF;
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
