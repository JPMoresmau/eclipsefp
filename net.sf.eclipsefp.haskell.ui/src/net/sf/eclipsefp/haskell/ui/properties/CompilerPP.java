// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.dialogs.PropertyPage;

/** <p>The property page for project-specific compiler settings.</p>
  * 
  * @author Leif Frenzel
  */
public class CompilerPP extends PropertyPage
                        implements IWorkbenchPreferencePage {
  
  // interface methods of IDialogPage
  ///////////////////////////////////
  
  public Control createContents( final Composite parent ) {
    Composite container = new Composite( parent, SWT.NULL );
    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    container.setLayout( layout );
    
    Label label = new Label( container, SWT.NONE );
    label.setText( "Not yet implemented." ); 
    
    Dialog.applyDialogFont( parent );
    return container;
  }
  
  
  // interface methods of IPreferencePage / IWorkbenchPreferencePage
  //////////////////////////////////////////////////////////////////
  
  public void init( final IWorkbench workbench ) {
    // unused
  }

  public boolean performOk() {
    return super.performOk();
  }

  public void performDefaults() {
    super.performDefaults();
  }
}