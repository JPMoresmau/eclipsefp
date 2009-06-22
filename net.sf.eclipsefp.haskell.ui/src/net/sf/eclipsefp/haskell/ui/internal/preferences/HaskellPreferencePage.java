// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences;


import net.sf.eclipsefp.common.ui.preferences.AbstractPreferencePage;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * A class similar to Eclipse's {@link FieldEditorPreferencePage},
 * but with more flexibility.
 *
 * @author Leif Frenzel
 * @author Thomas ten Cate
 */
public abstract class HaskellPreferencePage extends AbstractPreferencePage
implements IPreferenceConstants {

	@Override
	protected IPreferenceStore doGetPreferenceStore() {
		return HaskellUIPlugin.getDefault().getPreferenceStore();
	}

}