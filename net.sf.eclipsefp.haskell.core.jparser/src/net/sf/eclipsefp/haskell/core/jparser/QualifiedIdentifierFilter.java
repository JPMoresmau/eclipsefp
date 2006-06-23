package net.sf.eclipsefp.haskell.core.jparser;

import java.util.Hashtable;
import java.util.Map;

import antlr.CommonToken;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

/**
 * Filters an underlying token stream looking for haskell qualified identifiers
 * (for example: Char.isAlpha). When one is found, it is packaged as an unique
 * token and returned to the caller.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class QualifiedIdentifierFilter extends TokenStreamProcessor implements HaskellLexerTokenTypes {

	private static final Map<Integer, Integer> QUALIFIED_TYPE_TABLE
		= new Hashtable<Integer, Integer>();
	
	static {
		QUALIFIED_TYPE_TABLE.put(CONSTRUCTOR_ID, QCONID);
		QUALIFIED_TYPE_TABLE.put(VARIABLE_ID, QVARID);
		QUALIFIED_TYPE_TABLE.put(VARSYM, QVARSYM);
	}
	
	public QualifiedIdentifierFilter(TokenStream input) {
		super(new LookaheadTokenStream(input));
	}

	protected void insertTokensAsNeeded() throws TokenStreamException {
		if (!(isConstructorId(peekToken(1)) && isDot(peekToken(2))))
			return;
		
		if (  peekToken(2).getColumn()
		   != peekToken(1).getColumn() + peekToken(1).getText().length() )
			return;
		
		StringBuffer tokenText = new StringBuffer();
		while (isConstructorId(peekToken(1)) && isDot(peekToken(2))) {
			Token conid = consumeToken();
			Token dot = consumeToken();
			
			tokenText.append(conid.getText());
			tokenText.append(dot.getText());
		}
		Token id = consumeToken();
		tokenText.append(id.getText());
		
		Token qualifiedId = new CommonToken(getQualifiedType(id),
								tokenText.toString());
		
		insertToken(qualifiedId);
	}

	private int getQualifiedType(Token id) throws TokenStreamException {
		Integer result = QUALIFIED_TYPE_TABLE.get(id.getType());
		if (result == null) {
			final String msg = String.format(
					"line %d,%d: Invalid qualified token '%s'",
					id.getLine(), id.getColumn(), id.getText());
			throw new TokenStreamException(msg);
		}
		return result;
	}

	private boolean isDot(Token aToken) {
		return aToken.getType() == HaskellLexerTokenTypes.VARSYM
		    && ".".equals(aToken.getText());
	}

	private boolean isConstructorId(Token aToken) {
		return aToken.getType() == HaskellLexerTokenTypes.CONSTRUCTOR_ID;
	}

}
