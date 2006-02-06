package net.sf.eclipsefp.haskell.core.jparser;

import antlr.CommonToken;

public class EclipseFPToken extends CommonToken {

	private long fOffset;

	public EclipseFPToken() {
	}
	
	public EclipseFPToken(int type) {
		this(type, "<no text>");
	}

	public EclipseFPToken(int type, String text) {
		super(type, text);
	}

	public long getOffset() {
		return fOffset;
	}

	public void setOffset(long offset) {
		fOffset = offset;
	}

}
