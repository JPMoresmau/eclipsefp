package net.sf.eclipsefp.haskell.core.jparser;

import antlr.CommonToken;

public class EclipseFPToken extends CommonToken {

	private long fOffset;

	public long getOffset() {
		return fOffset;
	}

	public void setOffset(long offset) {
		fOffset = offset;
	}

}
