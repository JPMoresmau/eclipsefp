// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;



/** <p>A dialog field with a checkbox for boolean values</p>
  * 
  * @author Leif Frenzel
  */
public class BooleanDialogField extends DialogField {

  private Boolean info;
  // UI components
  private final Button checkBox;

  public BooleanDialogField( final Composite parent, 
                             final String text ) {
    super( parent );
    
    GridLayout gridLayout = new GridLayout( 3, false );
    setLayout( gridLayout );
    
    checkBox = new Button( this, SWT.CHECK );
    checkBox.setText( text );

    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.horizontalIndent = 0;
    gd.horizontalSpan = 2;
    checkBox.setLayoutData( gd );
    addListener( checkBox );
  }
  
  public BooleanDialogField( final Composite parent, 
                             final String text, 
                             final String tooltipText ) {
    this( parent, text );
    checkBox.setToolTipText( tooltipText );
  }
  
  
  // interface methods of DialogField
  ///////////////////////////////////

  @Override
  public void setInfo( final Object info ) {
    this.info = ( Boolean )info;
    checkBox.setSelection( this.info.booleanValue() );
  }
  
  @Override
  public Object getInfo() {
    return info;
  }
  
  
  // UI creation
  //////////////
  
  private void addListener( final Button checkBox ) {
    checkBox.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent event ) {
        Button button = ( Button )event.widget;
        boolean sel = button.getSelection();
        Object newInfo = ( sel ) ? Boolean.TRUE : Boolean.FALSE;
        notifyListeners( newInfo );
      }
    } );
  }
}