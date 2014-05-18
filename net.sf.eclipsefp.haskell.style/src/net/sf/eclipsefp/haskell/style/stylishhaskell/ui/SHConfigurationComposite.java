/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style.stylishhaskell.ui;

import net.sf.eclipsefp.haskell.style.stylishhaskell.SHConfiguration;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHImports;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHPragmas;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHRecords;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHTabs;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHTrailingWhitespace;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHUnicode;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHImports.SHImportAlign;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHPragmas.SHPragmaStyle;
import net.sf.eclipsefp.haskell.style.util.StyleText;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.model.WorkbenchViewerComparator;

/**
 * Composite to manage a stylish haskell configuration
 * @author JP Moresmau
 *
 */
public class SHConfigurationComposite extends Composite{
	private Button bUnicode;
	private Button bUnicodePragmas;	
	private Button bImports;
	private ComboViewer bImportsAlign;
	private Button bPragmas;
	private ComboViewer bPragmasStyle;
	private Button bPragmasRemove;	
	private Button bTabs;
	private Text tTabsSpaces;
	private Button bTrailing;
	private Button bRecords;
	private Text tCols;	
	
	public SHConfigurationComposite(Composite parent, int style) {
		super(parent, style);
		initUI();
	}

	private void styleMain(Control c){
		GridData gdUP=new GridData(GridData.FILL_HORIZONTAL);
		gdUP.horizontalSpan=2;
		c.setLayoutData(gdUP);
	}
	
	private void styleIndent(Control c,int span){
		GridData gdUP=new GridData();
		gdUP.horizontalIndent=30;
		gdUP.horizontalSpan=span;
		c.setLayoutData(gdUP);
		
	}
	
	private void initUI(){
		setLayout(new GridLayout(2,false));
		
		bUnicode=new Button(this,SWT.CHECK);
		bUnicode.setText(StyleText.sh_unicode);
		styleMain(bUnicode);
		
		bUnicodePragmas=new Button(this,SWT.CHECK);
		bUnicodePragmas.setText(StyleText.sh_unicode_pragmas);
		bUnicodePragmas.setSelection(true);
		styleIndent(bUnicodePragmas,2);
		
		bUnicode.addSelectionListener(new SelectionAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				bUnicodePragmas.setEnabled(bUnicode.getSelection());
			}
		});
		
		bImports=new Button(this,SWT.CHECK);
		bImports.setText(StyleText.sh_import);
		styleMain(bImports);
		
		final Label lImportsAlign=new Label(this, SWT.NONE);
		lImportsAlign.setText(StyleText.sh_import_alignment);
		styleIndent(lImportsAlign, 1);
				
		bImportsAlign=new ComboViewer(this);
		bImportsAlign.setComparator(new WorkbenchViewerComparator());
		bImportsAlign.setContentProvider(new ArrayContentProvider());
		bImportsAlign.setInput(SHImportAlign.values());
		bImportsAlign.setSelection(new StructuredSelection(SHImportAlign.GLOBAL));
		
		bImports.addSelectionListener(new SelectionAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				lImportsAlign.setEnabled(bImports.getSelection());
				bImportsAlign.getCombo().setEnabled(bImports.getSelection());
			}
		});
		
		bPragmas=new Button(this,SWT.CHECK);
		bPragmas.setText(StyleText.sh_pragmas);
		styleMain(bPragmas);
		
		final Label lPragmasStyle=new Label(this, SWT.NONE);
		lPragmasStyle.setText(StyleText.sh_pragmas_style);
		styleIndent(lPragmasStyle, 1);
				
		bPragmasStyle=new ComboViewer(this);
		bPragmasStyle.setComparator(new WorkbenchViewerComparator());
		bPragmasStyle.setContentProvider(new ArrayContentProvider());
		bPragmasStyle.setInput(SHPragmaStyle.values());
		bPragmasStyle.setSelection(new StructuredSelection(SHPragmaStyle.VERTICAL));
		
		bPragmasRemove=new Button(this,SWT.CHECK);
		bPragmasRemove.setText(StyleText.sh_pragmas_remove_redundant);
		bPragmasRemove.setSelection(true);
		styleIndent(bPragmasRemove,2);

		
		bPragmas.addSelectionListener(new SelectionAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				bPragmasRemove.setEnabled(bPragmas.getSelection());
				lPragmasStyle.setEnabled(bPragmas.getSelection());
				bPragmasStyle.getCombo().setEnabled(bPragmas.getSelection());
			}
		});
		
		bRecords=new Button(this,SWT.CHECK);
		bRecords.setText(StyleText.sh_records);
		styleMain(bRecords);
		
		bTabs=new Button(this,SWT.CHECK);
		bTabs.setText(StyleText.sh_tabs);
		styleMain(bTabs);
		
		final Label lTabsSpaces=new Label(this, SWT.NONE);
		lTabsSpaces.setText(StyleText.sh_tabs_spaces);
		styleIndent(lTabsSpaces, 1);
		
		tTabsSpaces=new Text(this,SWT.BORDER);
		tTabsSpaces.setText("8");
		GridData gdUP=new GridData();
		gdUP.widthHint=30;
		tTabsSpaces.setLayoutData(gdUP);
		
		tTabsSpaces.addVerifyListener(new VerifyListener() {
			
			@Override
			public void verifyText(VerifyEvent arg0) {
				if (Character.isDigit(arg0.character)){ 
					arg0.doit=true;
					return;
				}
				if (arg0.character=='\b'){
					arg0.doit=tTabsSpaces.getText().length()>1;
					return;
				}
				arg0.doit=false;
			}
		});
		
		bTabs.addSelectionListener(new SelectionAdapter() {
			/* (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				lTabsSpaces.setEnabled(bTabs.getSelection());
				tTabsSpaces.setEnabled(bTabs.getSelection());
			}
		});
		
		
		bTrailing=new Button(this,SWT.CHECK);
		bTrailing.setText(StyleText.sh_trailing_whitespace);
		styleMain(bTrailing);
		
		final Label lColsSpaces=new Label(this, SWT.NONE);
		lColsSpaces.setText(StyleText.sh_cols_spaces);
		
		tCols=new Text(this,SWT.BORDER);
		tCols.setText(String.valueOf(SHConfiguration.DEFAULT_COLUMNS));
		gdUP=new GridData();
		gdUP.widthHint=30;
		tCols.setLayoutData(gdUP);
		
		tCols.addVerifyListener(new VerifyListener() {
			
			@Override
			public void verifyText(VerifyEvent arg0) {
				if (arg0.character==0){
					try {
						Integer.parseInt(arg0.text);
						arg0.doit=true;
						return;
					} catch (NumberFormatException nfe){
						// down
					}
				}
				if (Character.isDigit(arg0.character)){ 
					arg0.doit=true;
					return;
				}
				if (arg0.character=='\b'){
					arg0.doit=tCols.getText().length()>1;
					return;
				}
				arg0.doit=false;
			}
		});
	}
	
	public void setConfiguration(SHConfiguration config){
		selectButton(bUnicode,config.getUnicode());
		if (config.getUnicode()!=null){
			bUnicodePragmas.setSelection(config.getUnicode().isUnicodePragmas());
		}
		
		selectButton(bImports,config.getImports());
		if (config.getImports()!=null){
			bImportsAlign.setSelection(new StructuredSelection(config.getImports().getAlign()));
		}
		
		selectButton(bPragmas,config.getPragmas());
		if (config.getPragmas()!=null){
			bPragmasStyle.setSelection(new StructuredSelection(config.getPragmas().getStyle()));
			bPragmasRemove.setSelection(config.getPragmas().isRemoveRedundant());
		} 
		
		selectButton(bRecords, config.getRecords());
		
		selectButton(bTabs,config.getTabs());
		if (config.getTabs()!=null){
			tTabsSpaces.setText(String.valueOf(config.getTabs().getSpaces()));
		} 
		
		selectButton(bTrailing,config.getTrailingWhitespace());
		
		tCols.setText(String.valueOf(config.getColumns()));
	}
	
	private void selectButton(Button b,Object o){
		b.setSelection(o!=null);
		b.notifyListeners(SWT.Selection, new Event());
	}
	
	public SHConfiguration getConfiguration(){
		SHConfiguration conf=new SHConfiguration();
		conf.clear();
		if (bUnicode.getSelection()){
			SHUnicode uni=new SHUnicode();
			uni.setUnicodePragmas(bUnicodePragmas.getSelection());
			conf.setUnicode(uni);
		}
		if (bImports.getSelection()){
			SHImports imps=new SHImports();
			SHImportAlign al=(SHImportAlign)((IStructuredSelection)bImportsAlign.getSelection()).getFirstElement();
			imps.setAlign(al);
			conf.setImports(imps);
		}
		if (bPragmas.getSelection()){
			SHPragmas pr=new SHPragmas();
			pr.setRemoveRedundant(bPragmasRemove.getSelection());
			SHPragmaStyle ps=(SHPragmaStyle)((IStructuredSelection)bPragmasStyle.getSelection()).getFirstElement();
			pr.setStyle(ps);
			conf.setPragmas(pr);
		}
		if (bRecords.getSelection()){
			conf.setRecords(new SHRecords());
		}
		if (bTabs.getSelection()){
			SHTabs tabs=new SHTabs();
			tabs.setSpaces(Integer.parseInt(tTabsSpaces.getText()));
			conf.setTabs(tabs);
		}
		if (bTrailing.getSelection()){
			conf.setTrailingWhitespace(new SHTrailingWhitespace());
		}
		if (tCols.getText().length()>0){
			conf.setColumns(Integer.parseInt(tCols.getText()));
		} else {
			conf.setColumns(SHConfiguration.DEFAULT_COLUMNS);
		}
		return conf;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.swt.widgets.Control#setEnabled(boolean)
	 */
	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		for (Control c:getChildren()) {
			c.setEnabled(enabled);
		}
		if (enabled){
			for (Control c:getChildren()) {
				c.notifyListeners(SWT.Selection, new Event());
			}
		}
	}
}
