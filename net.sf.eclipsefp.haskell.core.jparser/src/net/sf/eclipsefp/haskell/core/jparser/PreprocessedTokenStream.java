package net.sf.eclipsefp.haskell.core.jparser;

import antlr.CommonToken;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class PreprocessedTokenStream extends TokenStreamProcessor {

	private boolean fIsFirstCall = true;
	
	public PreprocessedTokenStream(final TokenStream stream) {
		super(stream);
	}
	
	@Override
  protected void insertTokensAsNeeded() throws TokenStreamException {
		if (hasInsertedTokens())
			return;
		
		if (fIsFirstCall) {
			Token firstToken = consumeLinebreaks();
			
			if (!isModule(firstToken) && !isLeftCurly(firstToken)) {
				insertOpenBlock(firstToken.getColumn());
			}
		} else if (isLineBreak(peekToken())) {
			//consume the line break char
			consumeToken();
			Token referenceToken = consumeLinebreaks();
			
			if (!isEOF(referenceToken)) {
				insertLineBreak(referenceToken.getColumn());
			}
		} else if (isBlockOpener(peekToken(1)) && !isLeftCurly(peekToken(2))) {
			insertToken(consumeToken());
			Token referenceToken = consumeLinebreaks();
			
			int openBlockColumn;
			
			if (isEOF(referenceToken)) {
				openBlockColumn = -1;
			} else {
				openBlockColumn = referenceToken.getColumn();
			}
			
			insertOpenBlock(openBlockColumn);
		}
		
		fIsFirstCall = false;
	}

	private void insertOpenBlock(final int column) throws TokenStreamException {
		insertControlToken(HaskellLexerExtendedTokenTypes.OPENBLOCK, column);
	}

	private void insertLineBreak(final int column) throws TokenStreamException {
		insertControlToken(HaskellLexerExtendedTokenTypes.LINEBREAK, column);
    }

	private void insertControlToken(final int tokenType, final int column) {
		Token controlToken = new CommonToken(tokenType, "<<special token>>");
		controlToken.setColumn(column);
		insertToken(controlToken);
	}

	private boolean isEOF(final Token token) {
		return token.getType() == HaskellLexerTokenTypes.EOF;
	}

	/**
	 * Consumes all line breaks on the underlying stream and returns
	 * the next non-linebreak token.
	 */
	private Token consumeLinebreaks() throws TokenStreamException {
		while(isLineBreak(peekToken())) {
			consumeToken();
		}
		return peekToken();
	}

	private boolean isLineBreak(final Token token) throws TokenStreamException {
		return token.getType() == HaskellLexerExtendedTokenTypes.NEWLINE;
	}
	
	private boolean isBlockOpener(final Token token) {
		return isWhere(token) || isLet(token) || isDo(token) || isOf(token);
	}

	private boolean isLet(final Token token) {
		return token.getType() == HaskellLexerTokenTypes.LET;
	}

	private boolean isDo(final Token token) {
		return token.getType() == HaskellLexerTokenTypes.DO;
	}

	private boolean isOf(final Token token) {
		return token.getType() == HaskellLexerTokenTypes.OF;
	}

	private boolean isWhere(final Token token) {
		return token.getType() == HaskellLexerTokenTypes.WHERE;
	}

	private boolean isLeftCurly(final Token theToken) {
		return theToken.getType() == HaskellLexerTokenTypes.LEFT_CURLY;
	}
	
	private boolean isModule(final Token token) {
		return token.getType() == HaskellLexerTokenTypes.MODULE;
	}
	
}
