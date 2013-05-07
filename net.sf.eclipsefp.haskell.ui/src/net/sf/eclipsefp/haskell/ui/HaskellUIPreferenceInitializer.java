/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui;


import net.sf.eclipsefp.haskell.ui.internal.preferences.DebugPP;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.preferences.NewHaskellProjectPP;
import net.sf.eclipsefp.haskell.ui.internal.preferences.SearchPathsPP;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.AbstractEditorPP;
import net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ScionPP;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

public class HaskellUIPreferenceInitializer extends
		AbstractPreferenceInitializer {

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore prefs = HaskellUIPlugin.getDefault().getPreferenceStore();
		//HaskellEditorPP.initializeDefaultValues(prefs);
		AbstractEditorPP.initializeDefaultValues( prefs );
		NewHaskellProjectPP.initializeDefaults(prefs);
		//BuildConsolePP.initializeDefaults(prefs);
		ScionPP.initializeDefaults(prefs);
		DebugPP.initializeDefaults( prefs );
		SearchPathsPP.initializeDefaults( prefs );

		prefs.setDefault( IPreferenceConstants.IGNORE_MISSING_EXECUTABLE, false );
		prefs.setDefault( IPreferenceConstants.HLINT_ALWAYS_SHOW_FULL_TEXT, false );
	}

}
