// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.ui.wizards;

import net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings.RenameModuleProcessor;
import net.sf.eclipsefp.haskell.refactoring.internal.core.refactorings.RenameModuleRefactoring;
import net.sf.eclipsefp.haskell.refactoring.internal.ui.HaskellRefactoringUI;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/** <p>the input page for the Rename Module refactoring, where users can
  * control the effects of the refactoring; to be shown in the wizard.</p>
  * 
  * @author Leif Frenzel
  */
public class RenameModuleInputPage extends UserInputWizardPage {

  private static final String DS_KEY = RenameModuleInputPage.class.getName();
  private static final String DS_UPDATE_REFS = "DS_UPDATE_REFS";
  
  private Text txtNewName;
  private IDialogSettings dialogSettings;
  private Button cbUpdateRefs;

  
  public RenameModuleInputPage() {
    super( "RenameRefactoringPage" );
    initDialogSettings();
  }

  
  // interface methods of UserInputWizardPage
  ///////////////////////////////////////////
  
  public void createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    GridLayout gridLayout = new GridLayout( 2, false );
    gridLayout.marginWidth = 10;
    gridLayout.marginHeight = 10;
    composite.setLayout( gridLayout );
    initializeDialogUnits( composite );
    Dialog.applyDialogFont( composite );
    setControl( composite );

    Label lblNewName = new Label( composite, SWT.NONE );
    lblNewName.setText( "New name: " );

    txtNewName = new Text( composite, SWT.BORDER );
    txtNewName.setText( getRenameProcessor().getInfo().getOldName() );
    txtNewName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    txtNewName.selectAll();

    txtNewName.addKeyListener( new KeyAdapter() {
      public void keyReleased( final KeyEvent e ) {
        getRenameProcessor().getInfo().setNewName( txtNewName.getText() );
        validate();
      }
    } );
    
    cbUpdateRefs = new Button( composite, SWT.CHECK );
    cbUpdateRefs.setText( "Update references" );
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalSpan = 2;
    cbUpdateRefs.setLayoutData( gridData );
    cbUpdateRefs.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent event ) {
        boolean selected = cbUpdateRefs.getSelection();
        dialogSettings.put( DS_UPDATE_REFS, selected );
        getRenameProcessor().getInfo().setUpdateReferences( selected );
      }
    } );
    
    initUpdateRefsOption();
    // TODO set only if text replace is requested
    getRefactoringWizard().setForcePreviewReview( true );
    validate();
  }


  // helping methods
  //////////////////

  private void validate() {
    String txt = txtNewName.getText();
    String oldName = getRenameProcessor().getInfo().getOldName();
    setPageComplete( txt.length() > 0 && !txt.equals( oldName ) );
  }
  
  private RenameModuleProcessor getRenameProcessor() {
    RenameModuleRefactoring ref = ( RenameModuleRefactoring )getRefactoring();
    return ( RenameModuleProcessor )ref.getProcessor();
  }
  
  private void initDialogSettings() {
    IDialogSettings ds = HaskellRefactoringUI.getDefault().getDialogSettings();
    dialogSettings = ds.getSection( DS_KEY );
    if( dialogSettings == null ) {
      dialogSettings = ds.addNewSection( DS_KEY );
      // init default values
      dialogSettings.put( DS_UPDATE_REFS, true );
    }
  }
  
  private void initUpdateRefsOption() {
    boolean updateRefs = dialogSettings.getBoolean( DS_UPDATE_REFS );
    cbUpdateRefs.setSelection( updateRefs );
    getRenameProcessor().getInfo().setUpdateReferences( updateRefs );
  }
}
