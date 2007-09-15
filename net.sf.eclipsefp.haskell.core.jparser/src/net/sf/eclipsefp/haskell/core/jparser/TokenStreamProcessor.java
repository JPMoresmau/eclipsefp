package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class TokenStreamProcessor implements TokenStream {

	private final Queue<Token> fInsertedTokens = new LinkedList<Token>();
	private final LookaheadTokenStream fInput;

	public TokenStreamProcessor(final TokenStream input) {
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

	protected Token peekToken(final int pos) throws TokenStreamException {
		return fInput.peekToken(pos);
	}

	protected boolean hasInsertedTokens() {
		return !fInsertedTokens.isEmpty();
	}

	protected void insertToken(final Token token) {
		fInsertedTokens.offer(token);
	}

}
