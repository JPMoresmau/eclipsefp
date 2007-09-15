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
package net.sf.eclipsefp.haskell.ui.preferences;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class BuildConsolePP extends PreferencePage implements
		IWorkbenchPreferencePage, IPreferenceConstants {

	public static void initializeDefaults(final IPreferenceStore store) {
		store.setDefault(CLEAR_BUILD_CONSOLE, true);
	}

	private Button fChkCleanConsole;
	private final IPreferenceStore fStore;
	private final HaskellPreferenceManager fManager;

	public BuildConsolePP() {
		this(HaskellUIPlugin.getDefault().getPreferenceManager(),
		     HaskellUIPlugin.getDefault().getPreferenceStore());
	}
	
	public BuildConsolePP(final HaskellPreferenceManager manager,
			              final IPreferenceStore store)
	{
		fManager = manager;
		fStore = store;
	}

	@Override
	protected Control createContents(final Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		
		composite.setLayout(new RowLayout());
		fChkCleanConsole = new Button(composite, SWT.CHECK);
		fChkCleanConsole.setText("Always clear console before building");
		
		createListeners();
		fillValuesInControls();
		return composite;
	}

	private void createListeners() {
		fChkCleanConsole.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(final SelectionEvent e) {
				//ignore event
			}

			public void widgetSelected(final SelectionEvent e) {
				fStore.setValue(CLEAR_BUILD_CONSOLE, fChkCleanConsole.getSelection());
			}
			
		});
	}

	private void fillValuesInControls() {
		fChkCleanConsole.setSelection(fStore.getBoolean(CLEAR_BUILD_CONSOLE));
	}

	
	@Override
	protected void performDefaults() {
		super.performDefaults();
		
		fStore.setToDefault(CLEAR_BUILD_CONSOLE);
		fillValuesInControls();
	}

	@Override
	public boolean performOk() {
		fManager.activateBuildConsolePreferences();
		
		return true;
	}

	public void init(final IWorkbench workbench) {
		//no need for initialization
	}

}
