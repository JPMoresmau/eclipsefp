package net.sf.eclipsefp.haskell.core.jparser.test.doubles;

import java.util.Iterator;
import java.util.List;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class ListTokenStream implements TokenStream {

	private Iterator<Token> fTokensImpl;
	
	public ListTokenStream(List<Token> tokens) {
		fTokensImpl = tokens.iterator();
	}

	public Token nextToken() throws TokenStreamException {
		return fTokensImpl.next();
	}

}
