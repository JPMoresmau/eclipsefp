package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.List;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class LookaheadTokenStream implements TokenStream {

	private final TokenStream fInput;
	private final List<Token> fLookaheadBuffer = new LinkedList<Token>();
	
	public LookaheadTokenStream(final TokenStream stream) {
		fInput = stream;
	}

	public Token nextToken() throws TokenStreamException {
		if (fLookaheadBuffer.isEmpty()) {
			return fInput.nextToken();
		} else {
			return fLookaheadBuffer.remove(0);
		}
	}

	public Token peekToken() throws TokenStreamException {
		return peekToken(1);
	}

	public Token peekToken(final int pos) throws TokenStreamException {
		final int realPos = pos - 1;
		final int bufferSize = fLookaheadBuffer.size();
		if (realPos >= bufferSize) {
			lookAhead(pos - bufferSize);
		}
		
		return fLookaheadBuffer.get(realPos);
	}

	private void lookAhead(final int numTokens) throws TokenStreamException {
		for(int i = 0; i < numTokens; ++i) {
			fLookaheadBuffer.add(fInput.nextToken());
		}
	}

}
