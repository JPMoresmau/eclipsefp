package net.sf.eclipsefp.haskell.core.jparser.test;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

/**
 * And enchanced token stream, customized for usage by testing code.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class TestTokenStream implements TokenStream {

	private TokenStream fStream;

	public TestTokenStream(TokenStream stream) {
		fStream = stream;
	}

	public Token nextToken() throws TokenStreamException {
		return fStream.nextToken();
	}

	/**
	 *  Consume <code>num</code> tokens from <code>stream</code>.
	 *  
	 * @param num    The number of tokens to consume
	 * @throws TokenStreamException
	 */
	public void skipTokens(int num) throws TokenStreamException {
		for(int i = 0; i < num; ++i) {
			nextToken(); 
		}
	}

}
