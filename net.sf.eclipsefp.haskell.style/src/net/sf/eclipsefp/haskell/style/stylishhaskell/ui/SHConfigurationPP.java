/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell.ui;

import net.sf.eclipsefp.haskell.style.stylishhaskell.StylishHaskell;
import net.sf.eclipsefp.haskell.style.util.StyleText;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PropertyPage;

/**
 * Property page to assign a stylish haskell configuration to a project
 * @author JP Moresmau
 *
 */
public class SHConfigurationPP extends PropertyPage implements
		IWorkbenchPreferencePage {
	private Button bProject;
	private SHConfigurationComposite confComp;
	
	/**
	 * 
	 */
	public SHConfigurationPP() {
		setDescription(StyleText.sh_title);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench arg0) {
		// NOOP

	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createContents(Composite parent) {
		Composite container = new Composite( parent, SWT.NULL );
	    GridLayout layout = new GridLayout();
	    layout.numColumns = 1;
	    container.setLayout( layout );
	    
	    bProject=new Button(container, SWT.CHECK);
	    bProject.setText(StyleText.sh_project);
	    
	    final Group g=new Group(parent,SWT.NONE);
	    //g.setText(StyleText.sh_title);
	    g.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING | GridData.GRAB_VERTICAL));
	    g.setLayout(new GridLayout(1,true));
	    
	    IProject project=( IProject )getElement();
	    confComp=new SHConfigurationComposite(g, SWT.NONE);
	    confComp.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING | GridData.GRAB_VERTICAL));
	    confComp.setConfiguration(StylishHaskell.getProjectConfiguration(project));
	    
	    bProject.addSelectionListener(new SelectionAdapter() {
	    	public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
	    		confComp.setEnabled(bProject.getSelection());
	    		g.setEnabled(bProject.getSelection());
	    	}
		});
	    
	    boolean hasConfig=StylishHaskell.hasProjectConfiguration(project);
	    bProject.setSelection(hasConfig);
	    confComp.setEnabled(hasConfig);
	    g.setEnabled(bProject.getSelection());
	    
	    Label space=new Label(parent,SWT.NONE);
	    space.setLayoutData(new GridData(GridData.FILL_VERTICAL));
	    
	    Dialog.applyDialogFont( parent );
	    return container;
	}

	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#performOk()
	 */
	@Override
	public boolean performOk() {
		IProject project=( IProject )getElement();
		try {
			if (bProject.getSelection()){
				StylishHaskell.setProjectConfiguration(confComp.getConfiguration(), project);
			} else {
				StylishHaskell.setProjectConfiguration(null, project);
			}
	    } catch (Exception ioe){
	        MessageDialog.openError( getShell(), StyleText.sh_save_error, ioe.getLocalizedMessage() );
	        return false;
	      }
		return super.performOk();
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
	 */
	@Override
	protected void performDefaults() {
		bProject.setSelection(false);
		confComp.setConfiguration(StylishHaskell.getWorkspaceConfiguration());
		bProject.notifyListeners(SWT.Selection, new Event());
		super.performDefaults();
	}
}
