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

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

public class BuildConsolePP extends PreferencePage implements
		IWorkbenchPreferencePage, IPreferenceConstants {

	public static void initializeDefaults(IPreferenceStore store) {
		store.setDefault(CLEAR_BUILD_CONSOLE, true);
	}

	private Button fChkCleanConsole;

	public BuildConsolePP() {
		//placeholder
	}

	public BuildConsolePP(String title) {
		super(title);
	}

	public BuildConsolePP(String title, ImageDescriptor image) {
		super(title, image);
	}

	@Override
	protected Control createContents(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		
		composite.setLayout(new RowLayout());
		fChkCleanConsole = new Button(composite, SWT.CHECK);
		fChkCleanConsole.setText("Always clear console before building");
		
		fChkCleanConsole.setSelection(true);
		return composite;
	}

	public void init(IWorkbench workbench) {
	}

	@Override
	protected void performDefaults() {
		// TODO Auto-generated method stub
		super.performDefaults();
	}

	@Override
	public boolean performOk() {
		// TODO Auto-generated method stub
		return super.performOk();
	}

}
