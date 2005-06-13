// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.dialog.WSFileDialogField;
import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/** <p>page for setting Haddock options.</p>
  *
  * @author Leif Frenzel
  */
class OptionsPage extends HaddockExportWizardPage {

  OptionsPage( final HaddockInfo info ) {
    super( "OptionsPage", info );
    setTitle( "Options" );
    String description = "Additional settings for the generation";
    setDescription( description );
  }
  
  
  // interface methods of IWizardPage
  ///////////////////////////////////
  
  public void createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );
    
    createTitleBlock( composite );
    // TODO some more options ...
    createPrologueBlock( composite );
    createCSSBlock( composite );
    
    setControl( composite );
  }

  
  // UI creation methods
  //////////////////////

  private void createTitleBlock( final Composite composite ) {
    final Button cbTitle = createCheckBox( composite, "Document title" );
    cbTitle.setSelection( getInfo().isUseTitle() );
    
    final Text txtTitle = new Text( composite, SWT.BORDER );
    txtTitle.setText( getInfo().getTitle() );
    txtTitle.setEnabled( getInfo().isUseTitle() );
    txtTitle.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    
    cbTitle.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbTitle.getSelection();
        txtTitle.setEnabled( selected );
        getInfo().setUseTitle( selected );
      }
    } );
    
    txtTitle.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent evt ) {
        getInfo().setTitle( txtTitle.getText() );
      }
    } );
  }

  private void createCSSBlock( final Composite composite ) {
    final Button cbCSS = createCheckBox( composite, "Style sheet" );
    cbCSS.setSelection( getInfo().isUseCssFile() );
    
    String filter = "css";
    final WSFileDialogField dlgField 
      = new WSFileDialogField( composite, "", filter );
    dlgField.setEnabled( getInfo().isUseCssFile() );
    dlgField.setInfo( getInfo().getCssFile() );
    dlgField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    
    cbCSS.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbCSS.getSelection();
        dlgField.setEnabled( selected );
        getInfo().setUseCssFile( selected );
        validate();
      }
    } );
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getInfo().setCssFile( ( String )newInfo );
        validate();
      }
    } );
  }

  private void createPrologueBlock( final Composite composite ) {
    final Button cbPrologue = createCheckBox( composite, "Prologue file" );
    cbPrologue.setSelection( getInfo().isUsePrologueFile() );

    final WSFileDialogField dlgField = new WSFileDialogField( composite, "" );
    dlgField.setEnabled( getInfo().isUsePrologueFile() );
    dlgField.setInfo( getInfo().getPrologueFile() );
    dlgField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    
    cbPrologue.addSelectionListener( new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbPrologue.getSelection();
        dlgField.setEnabled( selected );
        getInfo().setUsePrologueFile( selected );
        validate();
      }
    } );
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getInfo().setPrologueFile( ( String )newInfo );
        validate();
      }
    } );
  }
  
  
  // helping methods
  //////////////////
  
  private void validate() {
    List statusList = new ArrayList();
    if( getInfo().isUseCssFile() ) {
      statusList.add( validateFile( getInfo().getCssFile(), "Css file" ) );
    }
    if( getInfo().isUsePrologueFile() ) {
      String prologueFile = getInfo().getPrologueFile();
      statusList.add( validateFile( prologueFile, "Prologue file" ) );
    }
    apply( statusList );
  }
}
