package net.sf.eclipsefp.haskell.ui.util.preferences;

import org.eclipse.jface.preference.IPreferenceStore;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.preferences.editor.IEditorPreferenceNames;

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
