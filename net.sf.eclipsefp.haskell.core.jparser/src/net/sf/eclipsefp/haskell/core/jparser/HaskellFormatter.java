package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

import com.sun.org.apache.bcel.internal.generic.FCONST;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class HaskellFormatter implements TokenStream {

	private Stack<Integer> fLayoutContextStack = new Stack<Integer>();
	private Stack<Token> fOpeningTokenStack = new Stack<Token>();
	private Queue<Token> fInsertedTokens = new LinkedList<Token>();
	
	private LookaheadTokenStream fInput;

	public HaskellFormatter(TokenStream in) {
		fInput = new LookaheadTokenStream(new PreprocessedTokenStream(in));
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
		
		if (isOpenBlock(fInput.peekToken())) {
			Token openBlockToken = fInput.nextToken();
			
			if (!fLayoutContextStack.isEmpty() && openBlockToken.getColumn() > fLayoutContextStack.peek()) {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
				fLayoutContextStack.push(openBlockToken.getColumn());
			} else if (fLayoutContextStack.isEmpty() && openBlockToken.getColumn() > -1) {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
				fLayoutContextStack.push(openBlockToken.getColumn());
			} else {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.RIGHT_CURLY));
			}
			
		} else if (isLineBreak(fInput.peekToken())) {
			Token lineBreakToken = fInput.nextToken();
			
			if (!fLayoutContextStack.isEmpty() && lineBreakToken.getColumn() == fLayoutContextStack.peek()) {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.SEMICOLON));
			} else if (!fLayoutContextStack.isEmpty() && lineBreakToken.getColumn() < fLayoutContextStack.peek()) {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.RIGHT_CURLY));
				fLayoutContextStack.pop();
			}
		} else if (isEof(fInput.peekToken())) {
			if (!fLayoutContextStack.isEmpty() && fLayoutContextStack.peek() != -1) {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.RIGHT_CURLY));
				fLayoutContextStack.pop();
			}
		}
		
	}

	private boolean isLineBreak(Token token) {
		return token.getType() == HaskellLexerExtendedTokenTypes.LINEBREAK;
	}

	private boolean isOpenBlock(Token token) {
		return token.getType() == HaskellLexerExtendedTokenTypes.OPENBLOCK;
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
