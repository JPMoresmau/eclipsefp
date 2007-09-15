package net.sf.eclipsefp.haskell.core.jparser;

import antlr.TokenStream;
import antlr.TokenStreamBasicFilter;

public class HaskellCommentFilter extends TokenStreamBasicFilter {

	public HaskellCommentFilter(final TokenStream input) {
		super(input);
		
		this.discard(HaskellLexerTokenTypes.COMMENT);
	}

}
