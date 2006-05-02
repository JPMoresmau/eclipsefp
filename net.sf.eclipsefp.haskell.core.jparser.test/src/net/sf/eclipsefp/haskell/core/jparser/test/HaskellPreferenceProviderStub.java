package net.sf.eclipsefp.haskell.core.jparser.test;

import net.sf.eclipsefp.haskell.ui.util.preferences.IHaskellPreferenceProvider;

public class HaskellPreferenceProviderStub
	implements IHaskellPreferenceProvider {

	private int fTabSize = 8;

	public void setTabSize(int size) {
		fTabSize = size;
	}

	public int getTabSize() {
		return fTabSize;
	}

}
