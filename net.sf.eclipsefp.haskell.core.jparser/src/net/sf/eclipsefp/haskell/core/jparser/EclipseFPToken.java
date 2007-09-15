package net.sf.eclipsefp.haskell.core.jparser;

import antlr.CommonToken;

public class EclipseFPToken extends CommonToken {

	private long fOffset;

	public EclipseFPToken() {
	}
	
	public EclipseFPToken(final int type) {
		this(type, "<no text>");
	}

	public EclipseFPToken(final int type, final String text) {
		super(type, text);
	}

	public long getOffset() {
		return fOffset;
	}

	public void setOffset(final long offset) {
		fOffset = offset;
	}

}
