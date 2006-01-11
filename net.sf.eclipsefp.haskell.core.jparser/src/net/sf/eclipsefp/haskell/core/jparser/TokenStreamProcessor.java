package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class TokenStreamProcessor implements TokenStream {

	protected Queue<Token> fInsertedTokens = new LinkedList<Token>();
	private LookaheadTokenStream fInput;

	public TokenStreamProcessor(TokenStream input) {
		fInput = new LookaheadTokenStream(input);
	}
	
	public Token nextToken() throws TokenStreamException {
		insertTokensAsNeeded();
		
		if (fInsertedTokens.isEmpty()) {
			return fInput.nextToken();
		} else {
			return fInsertedTokens.poll();
		}
	}
	
	protected void insertTokensAsNeeded() throws TokenStreamException {
	}

	protected Token consumeToken() throws TokenStreamException {
		return fInput.nextToken();
	}
	
	protected Token peekToken() throws TokenStreamException {
		return fInput.peekToken();
	}

	protected Token peekToken(int pos) throws TokenStreamException {
		return fInput.peekToken(pos);
	}

	protected boolean hasInsertedTokens() {
		return !fInsertedTokens.isEmpty();
	}

	protected void insertToken(Token token) {
		fInsertedTokens.offer(token);
	}

}
