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

import net.sf.eclipsefp.haskell.ui.preferences.BuildConsolePP;
import net.sf.eclipsefp.haskell.ui.preferences.HaskellCompilerPP;
import net.sf.eclipsefp.haskell.ui.preferences.NewHaskellProjectPP;
import net.sf.eclipsefp.haskell.ui.preferences.editor.HaskellEditorPP;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

public class HaskellUIPreferenceInitializer extends
		AbstractPreferenceInitializer {

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore prefs = HaskellUIPlugin.getDefault().getPreferenceStore();
		HaskellCompilerPP.initializeDefaultValues(prefs);
		HaskellEditorPP.initializeDefaultValues(prefs);
		NewHaskellProjectPP.initializeDefaults(prefs);
		BuildConsolePP.initializeDefaults(prefs);
		
	}

}
