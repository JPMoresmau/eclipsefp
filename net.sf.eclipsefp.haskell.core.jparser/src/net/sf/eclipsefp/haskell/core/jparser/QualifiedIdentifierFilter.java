package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;

import antlr.CommonToken;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class QualifiedIdentifierFilter implements TokenStream {

	private LookaheadTokenStream fStream;
	private Queue<Token> fInsertedTokens = new LinkedList<Token>();
	
	public QualifiedIdentifierFilter(TokenStream input) {
		fStream = new LookaheadTokenStream(input);
	}

	public Token nextToken() throws TokenStreamException {
		insertNeededTokens();
		
		if (fInsertedTokens.isEmpty()) {
			return fStream.nextToken();
		} else {
			return fInsertedTokens.poll();
		}
	}

	private void insertNeededTokens() throws TokenStreamException {
		if (isConstructorId(fStream.peekToken()) && isDot(fStream.peekToken(2))) {
			Token conid = fStream.nextToken();
			Token dot = fStream.nextToken();
			Token varid = fStream.nextToken();
			Token qvarid = new CommonToken(HaskellLexerTokenTypes.QVARID,
					               conid.getText() + dot.getText() + varid.getText());
			fInsertedTokens.offer(qvarid);
		}
	}

	private boolean isDot(Token aToken) {
		return aToken.getType() == HaskellLexerTokenTypes.DOT;
	}

	private boolean isConstructorId(Token aToken) {
		return aToken.getType() == HaskellLexerTokenTypes.CONSTRUCTOR_ID;
	}

}
