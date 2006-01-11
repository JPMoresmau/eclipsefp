package net.sf.eclipsefp.haskell.core.jparser;

import java.util.Hashtable;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;

import antlr.CommonToken;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class QualifiedIdentifierFilter implements TokenStream, HaskellLexerTokenTypes {

	private static final Map<Integer, Integer> QUALIFIED_TYPE_TABLE
		= new Hashtable<Integer, Integer>();
	
	static {
		QUALIFIED_TYPE_TABLE.put(CONSTRUCTOR_ID, QCONID);
		QUALIFIED_TYPE_TABLE.put(VARIABLE_ID, QVARID);
		QUALIFIED_TYPE_TABLE.put(VARSYM, QVARSYM);
	}
	
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
			Token id = fStream.nextToken();
			String tokenText = conid.getText() + dot.getText() + id.getText();
			
			Token qualifiedId = new CommonToken(getQualifiedType(id),
									tokenText);
			fInsertedTokens.offer(qualifiedId);
		}
	}

	private int getQualifiedType(Token id) {
		return QUALIFIED_TYPE_TABLE.get(id.getType());
	}

	private boolean isDot(Token aToken) {
		return aToken.getType() == HaskellLexerTokenTypes.DOT;
	}

	private boolean isConstructorId(Token aToken) {
		return aToken.getType() == HaskellLexerTokenTypes.CONSTRUCTOR_ID;
	}

}
