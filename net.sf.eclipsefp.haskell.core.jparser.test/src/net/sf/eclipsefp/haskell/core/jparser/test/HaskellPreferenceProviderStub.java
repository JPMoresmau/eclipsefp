package net.sf.eclipsefp.haskell.core.jparser.test;

import de.leiffrenzel.fp.haskell.ui.util.preferences.IHaskellPreferenceProvider;

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
