package net.sf.eclipsefp.haskell.ui.test.util.preferences;

import org.eclipse.jface.preference.IPreferenceStore;

import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;
import de.leiffrenzel.fp.haskell.ui.preferences.editor.IEditorPreferenceNames;
import de.leiffrenzel.fp.haskell.ui.util.preferences.HaskellPreferenceProvider;
import de.leiffrenzel.fp.haskell.ui.util.preferences.IHaskellPreferenceProvider;
import junit.framework.TestCase;

public class HaskellPreferenceProvider_PDETest extends TestCase
											   implements IEditorPreferenceNames
{
	
	public void testTabSize() {
		final IHaskellPreferenceProvider prefs = new HaskellPreferenceProvider();

		int tabSize = 6;
		setPreferenceValue(EDITOR_TAB_WIDTH, tabSize);
		
		assertEquals(tabSize, prefs.getTabSize());
		
		tabSize = 4;
		setPreferenceValue(EDITOR_TAB_WIDTH, tabSize);
		assertEquals(tabSize, prefs.getTabSize());
	}

	private void setPreferenceValue(String key, int value) {
		getPreferenceStore().setValue(key, value);
	}
	
    private IPreferenceStore getPreferenceStore() {
    	return HaskellUIPlugin.getDefault().getPreferenceStore();
	}
	

}
