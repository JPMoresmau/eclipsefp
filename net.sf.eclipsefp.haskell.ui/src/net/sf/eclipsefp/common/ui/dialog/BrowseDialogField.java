// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import net.sf.eclipsefp.common.ui.util.DialogUtil;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/** <p>the superclass of dialog fields that offer browsing to a file or 
  * directory.</p>
  *
  * @author Leif Frenzel
  */
abstract class BrowseDialogField extends DialogField {

  private String info;

  // UI components
  private final Text text;
  private Button btnBrowse;
  
  public BrowseDialogField( final Composite parent, 
                            final String labelText ) {
    super( parent );
    GridLayout layout = new GridLayout();
    layout.numColumns = 3;
    setLayout( layout );
    initLabel( labelText );
    text = new Text( this, SWT.BORDER | SWT.SINGLE );
    configureLayout();
    addModifyListener();
    addFileField();
  }
  
  @Override
  public void setEnabled( final boolean enabled ) {
    super.setEnabled( enabled );
    text.setEnabled( enabled );
    btnBrowse.setEnabled( enabled );
  }

  
  // interface methods of DialogField 
  ///////////////////////////////////

  @Override
  public Object getInfo() {
    return info;
  }  
   
  @Override
  public void setInfo( final Object info ) {
    this.info = ( String )info;
    text.setText( this.info );  
  }
  

  // UI creation
  //////////////
  
  private void initLabel( final String labelText ) {
    Label labelControl = new Label( this, SWT.NONE );
    labelControl.setText( labelText );
    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.horizontalIndent = 1;
    labelControl.setLayoutData( gd );
  }

  private void addModifyListener() {
    text.addModifyListener( new ModifyListener() {
      @Override
      public void modifyText( final ModifyEvent event ) {
        Text text = ( Text )event.widget;
        info = text.getText();
        notifyListeners( info );
      }
    } );
  }
  
  private void configureLayout() {
    int width = 40;
    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.widthHint = DialogUtil.convertWidthInCharsToPixels( text, width + 1 ); 
    text.setLayoutData( gd );
  }
  
  private void addFileField() {
    btnBrowse = new Button( this, SWT.PUSH );
    btnBrowse.setText( "Browse..." );
    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.widthHint = DialogUtil.convertWidthInCharsToPixels( btnBrowse, 13 );
    btnBrowse.setLayoutData( gd );
    btnBrowse.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        String selectedFile = openDialog( text.getShell() );
        if( selectedFile != null ) {
          text.setText( selectedFile );
        }
      }
    } );
  }

  abstract String openDialog( Shell shell );
}
