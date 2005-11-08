// $ANTLR 2.7.5 (20051010): "haskell-parser.g" -> "HaskellParser.java"$

	package net.sf.eclipsefp.haskell.core.jparser;

import antlr.TokenBuffer;
import antlr.TokenStreamException;
import antlr.TokenStreamIOException;
import antlr.ANTLRException;
import antlr.LLkParser;
import antlr.Token;
import antlr.TokenStream;
import antlr.RecognitionException;
import antlr.NoViableAltException;
import antlr.MismatchedTokenException;
import antlr.SemanticException;
import antlr.ParserSharedInputState;
import antlr.collections.impl.BitSet;

public class HaskellParser extends antlr.LLkParser       implements HaskellParserTokenTypes
 {

protected HaskellParser(TokenBuffer tokenBuf, int k) {
  super(tokenBuf,k);
  tokenNames = _tokenNames;
}

public HaskellParser(TokenBuffer tokenBuf) {
  this(tokenBuf,1);
}

protected HaskellParser(TokenStream lexer, int k) {
  super(lexer,k);
  tokenNames = _tokenNames;
}

public HaskellParser(TokenStream lexer) {
  this(lexer,1);
}

public HaskellParser(ParserSharedInputState state) {
  super(state,1);
  tokenNames = _tokenNames;
}

	public final void parseModule() throws RecognitionException, TokenStreamException {
		
		
		try {      // for error handling
			switch ( LA(1)) {
			case MODULE:
			{
				match(MODULE);
				match(CONSTRUCTOR_ID);
				match(WHERE);
				body();
				break;
			}
			case LEFT_CURLY:
			{
				body();
				break;
			}
			default:
			{
				throw new NoViableAltException(LT(1), getFilename());
			}
			}
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_0);
		}
	}
	
	public final void body() throws RecognitionException, TokenStreamException {
		
		
		try {      // for error handling
			match(LEFT_CURLY);
			{
			_loop5:
			do {
				switch ( LA(1)) {
				case WS:
				case MODULE:
				case WHERE:
				case CONSTRUCTOR_ID:
				case VARIABLE_ID:
				case DECIMAL:
				case SEMICOLON:
				case UPPER_CASE:
				case LOWER_CASE:
				case LETTER:
				case DIGIT:
				case SYMBOL:
				{
					{
					match(_tokenSet_1);
					}
					break;
				}
				case LEFT_CURLY:
				{
					body();
					break;
				}
				default:
				{
					break _loop5;
				}
				}
			} while (true);
			}
			match(RIGHT_CURLY);
		}
		catch (RecognitionException ex) {
			reportError(ex);
			recover(ex,_tokenSet_2);
		}
	}
	
	
	public static final String[] _tokenNames = {
		"<0>",
		"EOF",
		"<2>",
		"NULL_TREE_LOOKAHEAD",
		"WS",
		"MODULE",
		"WHERE",
		"CONSTRUCTOR_ID",
		"VARIABLE_ID",
		"DECIMAL",
		"LEFT_CURLY",
		"RIGHT_CURLY",
		"SEMICOLON",
		"UPPER_CASE",
		"LOWER_CASE",
		"LETTER",
		"DIGIT",
		"SYMBOL"
	};
	
	private static final long[] mk_tokenSet_0() {
		long[] data = { 2L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_0 = new BitSet(mk_tokenSet_0());
	private static final long[] mk_tokenSet_1() {
		long[] data = { 259056L, 0L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_1 = new BitSet(mk_tokenSet_1());
	private static final long[] mk_tokenSet_2() {
		long[] data = { 262130L, 0L, 0L, 0L};
		return data;
	}
	public static final BitSet _tokenSet_2 = new BitSet(mk_tokenSet_2());
	
	}
