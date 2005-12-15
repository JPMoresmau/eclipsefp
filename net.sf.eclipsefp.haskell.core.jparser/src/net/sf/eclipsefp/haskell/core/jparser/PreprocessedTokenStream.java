package net.sf.eclipsefp.haskell.core.jparser;

import java.util.LinkedList;
import java.util.Queue;

import antlr.CommonToken;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

public class PreprocessedTokenStream implements TokenStream {

	private Queue<Token> fInsertedTokens = new LinkedList<Token>();
	private LookaheadTokenStream fStream;
	private boolean fIsFirstCall = true;
	
	public PreprocessedTokenStream(TokenStream stream) {
		fStream = new LookaheadTokenStream(stream);
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
		if (!fInsertedTokens.isEmpty())
			return;
		
		if (isLineBreak(fStream.peekToken())) {
			fStream.nextToken();
			Token referenceToken = nextNonLinebreak(fStream);
			
			Token lineBreakToken = new CommonToken(HaskellLexerExtendedTokenTypes.LINEBREAK, "<no text>");
			lineBreakToken.setColumn(referenceToken.getColumn());
			
			fInsertedTokens.offer(lineBreakToken);
		} else if (fIsFirstCall && !isModule(fStream.peekToken()) && !isLeftCurly(fStream.peekToken())) {
			Token referenceToken = nextNonLinebreak(fStream);

			Token openBlockToken = new CommonToken(HaskellLexerExtendedTokenTypes.OPENBLOCK, "<no text>");
			openBlockToken.setColumn(referenceToken.getColumn());
			fInsertedTokens.offer(openBlockToken);
		} else if (isBlockOpener(fStream.peekToken(1)) && !isLeftCurly(fStream.peekToken(2))) {
			fInsertedTokens.offer(fStream.nextToken());
			Token referenceToken = nextNonLinebreak(fStream);
			
			Token openBlockToken = new CommonToken(HaskellLexerExtendedTokenTypes.OPENBLOCK, "<no text>");
			if (isEOF(referenceToken)) {
				openBlockToken.setColumn(-1);
			} else {
				openBlockToken.setColumn(referenceToken.getColumn());
			}
			fInsertedTokens.offer(openBlockToken);
		}
		
		fIsFirstCall = false;
	}

	private boolean isEOF(Token token) {
		return token.getType() == HaskellLexerTokenTypes.EOF;
	}

	private Token nextNonLinebreak(LookaheadTokenStream stream) throws TokenStreamException {
		int i = 1;
		while(true) {
			Token t = stream.peekToken(i++);
			if (!isLineBreak(t))
				return t;
		}
	}

	private boolean isLineBreak(Token token) throws TokenStreamException {
		return token.getType() == HaskellLexerExtendedTokenTypes.NEWLINE;
	}
	
	private boolean isBlockOpener(Token token) {
		return isWhere(token) || isLet(token) || isDo(token) || isOf(token);
	}

	private boolean isLet(Token token) {
		return token.getType() == HaskellLexerTokenTypes.LET;
	}

	private boolean isDo(Token token) {
		return token.getType() == HaskellLexerTokenTypes.DO;
	}

	private boolean isOf(Token token) {
		return token.getType() == HaskellLexerTokenTypes.OF;
	}

	private boolean isWhere(Token token) {
		return token.getType() == HaskellLexerTokenTypes.WHERE;
	}

	private boolean isLeftCurly(Token theToken) {
		return theToken.getType() == HaskellLexerTokenTypes.LEFT_CURLY;
	}
	
	private boolean isModule(Token token) {
		return token.getType() == HaskellLexerTokenTypes.MODULE;
	}
	
}
