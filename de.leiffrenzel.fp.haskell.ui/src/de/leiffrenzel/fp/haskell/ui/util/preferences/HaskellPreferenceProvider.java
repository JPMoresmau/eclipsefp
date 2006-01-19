package de.leiffrenzel.fp.haskell.ui.util.preferences;

import org.eclipse.jface.preference.IPreferenceStore;

import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;
import de.leiffrenzel.fp.haskell.ui.preferences.editor.IEditorPreferenceNames;

public class HaskellPreferenceProvider implements IHaskellPreferenceProvider,
                                                  IEditorPreferenceNames
{

	public int getTabSize() {
		return getPreferenceStore().getInt(EDITOR_TAB_WIDTH);
	}

    private IPreferenceStore getPreferenceStore() {
    	return HaskellUIPlugin.getDefault().getPreferenceStore();
	}

}
