package net.sf.eclipsefp.haskell.core.jparser.test;

import antlr.CommonToken;
import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;
import net.sf.eclipsefp.haskell.core.jparser.QualifiedIdentifierFilter;

public class QualifiedIdentifierFilterTest extends TokenStreamTestCase implements HaskellLexerTokenTypes {
	
	public void testQualifiedVariable() throws TokenStreamException {
		TokenStream input = new TestTokenStream(new CommonToken(CONSTRUCTOR_ID, "MyModule"),
									new CommonToken(DOT, "."),
									new CommonToken(VARIABLE_ID, "aFunction"));
		TokenStream filter = new QualifiedIdentifierFilter(input);
		assertToken(QVARID, "MyModule.aFunction", filter.nextToken());
	}
	
	private static class TestTokenStream implements TokenStream {

		private Token[] fTokens;
		private int fCurrentToken = 0;
		private final Token EOF = new CommonToken(HaskellLexerTokenTypes.EOF, "<<eof>>");
		
		public TestTokenStream(Token... tokens) {
			fTokens = tokens;
		}
		
		public Token nextToken() throws TokenStreamException {
			if (fCurrentToken < fTokens.length) {
				return fTokens[fCurrentToken++];
			} else {
				return EOF;
			}
		}
		
	}

}
