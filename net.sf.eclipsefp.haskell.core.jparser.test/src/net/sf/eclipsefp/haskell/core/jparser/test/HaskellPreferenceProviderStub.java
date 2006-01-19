package net.sf.eclipsefp.haskell.core.jparser.test;

import net.sf.eclipsefp.haskell.core.jparser.IHaskellPreferenceProvider;

public class HaskellPreferenceProviderStub
	implements IHaskellPreferenceProvider {

	private int fTabSize;

	public void setTabSize(int size) {
		fTabSize = size;
	}

	public int getTabSize() {
		return fTabSize;
	}

}
