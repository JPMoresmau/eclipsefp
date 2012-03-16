// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;



/** <p>A dialog field with a textfield for String values.</p>
  * 
  * @author Leif Frenzel
  */
public class StringDialogField extends DialogField {

  private String info;
  // UI components
  private final Text textField;

  public StringDialogField( final Composite parent, 
                            final String text ) {
    super( parent );
    
    GridLayout gridLayout = new GridLayout( 3, false );
    setLayout( gridLayout );
    
    textField = new Text( this, SWT.BORDER );
    addListener( textField );
    
    if( !text.equals( "" ) ) {
      Label label = new Label( parent, SWT.NONE );
      GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
      gd.horizontalIndent = 0;
      gd.horizontalSpan = 2;
      label.setLayoutData( gd );
      label.setText( text );
    }
  }
  
  
  // interface methods of DialogField
  ///////////////////////////////////

  @Override
  public void setInfo( final Object info ) {
    this.info = ( String )info;
    textField.setText( this.info );
  }
  
  @Override
  public Object getInfo() {
    return info;
  }
  
  
  // UI creation
  //////////////
  
  private void addListener( final Text text ) {
    text.addModifyListener( new ModifyListener() {
      @Override
      public void modifyText( final ModifyEvent event ) {
        Text widget = ( Text )event.widget;
        Object newInfo = widget.getText();
        notifyListeners( newInfo );
      }
    } );
  }
}