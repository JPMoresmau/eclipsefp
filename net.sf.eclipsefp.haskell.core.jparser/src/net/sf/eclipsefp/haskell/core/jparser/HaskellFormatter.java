package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class HaskellFormatter implements TokenStream {

	private Stack<Integer> fLayoutContextStack = new Stack<Integer>();
	private Queue<Token> fInsertedTokens = new LinkedList<Token>();
	
	private LookaheadTokenStream fInput;

	public HaskellFormatter(TokenStream in) {
		fInput = new LookaheadTokenStream(
				  new PreprocessedTokenStream(
				   new HaskellCommentFilter(in)));
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
			Token openBlockToken = fInput.peekToken();
			
			if (!fLayoutContextStack.isEmpty() && openBlockToken.getColumn() > fLayoutContextStack.peek()) {
				fInput.nextToken();
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
				fLayoutContextStack.push(openBlockToken.getColumn());
			} else if (fLayoutContextStack.isEmpty() && openBlockToken.getColumn() > -1) {
				fInput.nextToken();
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
				fLayoutContextStack.push(openBlockToken.getColumn());
			} else {
				openBlockToken.setType(HaskellLexerExtendedTokenTypes.LINEBREAK);
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.LEFT_CURLY));
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.RIGHT_CURLY));
			}
			
		} else if (isLineBreak(fInput.peekToken())) {
			Token lineBreakToken = fInput.peekToken();
			
			if (!fLayoutContextStack.isEmpty() && lineBreakToken.getColumn() == fLayoutContextStack.peek()) {
				fInput.nextToken();
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.SEMICOLON));
			} else if (!fLayoutContextStack.isEmpty() && lineBreakToken.getColumn() < fLayoutContextStack.peek()) {
				fInsertedTokens.offer(new Token(HaskellLexerTokenTypes.RIGHT_CURLY));
				fLayoutContextStack.pop();
			} else {
				fInput.nextToken();
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

}
