package net.sf.eclipsefp.haskell.core.jparser;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class LookaheadTokenStream implements TokenStream {

	private TokenStream fInput;
	private Token fLookaheadToken = null;
	
	public LookaheadTokenStream(TokenStream stream) {
		fInput = stream;
	}

	public Token nextToken() throws TokenStreamException {
		Token result;
		if (fLookaheadToken == null) {
			result = fInput.nextToken();
		} else {
			result = fLookaheadToken;
			fLookaheadToken = null;
		}
		return result;
	}

	public Token peekToken() throws TokenStreamException {
		if (fLookaheadToken == null) {
			fLookaheadToken = fInput.nextToken();
		}
			
		return fLookaheadToken;
	}

}
