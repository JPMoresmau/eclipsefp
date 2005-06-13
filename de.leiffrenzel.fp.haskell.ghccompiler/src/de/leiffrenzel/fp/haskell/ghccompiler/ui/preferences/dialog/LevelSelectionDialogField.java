// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.ui.preferences.dialog;

import net.sf.eclipsefp.common.ui.dialog.DialogField;

import org.eclipse.jface.util.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

import de.leiffrenzel.fp.haskell.ghccompiler.core.IGhcParameters;
import de.leiffrenzel.fp.haskell.ghccompiler.ui.preferences.UITexts;



/** <p>dialog field for the selection of compiler optimization levels.</p>
  * 
  * @author Leif Frenzel
  */
public class LevelSelectionDialogField extends DialogField 
                                       implements IGhcParameters {

  private int selected = -1;
  // ui elements
  private Button[] buttons;
  
  public LevelSelectionDialogField( final Composite parent ) {
    super( parent );
    setLayout( new GridLayout( 1, false ) );
    createLabel();
    createButtons();
  }
  

  // interface methods of DialogField
  ///////////////////////////////////
  
  public Object getInfo() {
    return new Integer( selected );
  }

  public void setInfo( final Object info ) {
    int newSelected = ( ( Integer )info ).intValue();
    Assert.isTrue( newSelected >= -1 && newSelected <= 2 );
    selected = newSelected;
    applySelection();
  }
  
  
  // UI creation methods
  //////////////////////
  
  private Button createButton( final Composite parent, final String prefKey ) {
    String text = UITexts.getShortDescription( prefKey );
    String tooltip = text + "\n" + UITexts.getOption( prefKey );
    return createButton( parent, text, tooltip );
  }
  
  private Button createButton( final Composite parent, 
                               final String text,
                               final String tooltip) {
    Button result = new Button( parent, SWT.RADIO | SWT.LEFT );
    result.setText( text );
    result.setToolTipText( tooltip );
    return result;
  }
  
  private void createButtons() {
    String text = "Do not specify an optimization level.";
    String tooltip = text + "\nPass no -O* option.";
    
    buttons = new Button[ 4 ];
    buttons[ 0 ] = createButton( this, text, tooltip );
    buttons[ 1 ] = createButton( this, OPT_O0 );
    buttons[ 2 ] = createButton( this, OPT_O1 );
    buttons[ 3 ] = createButton( this, OPT_O2 );
    SelectionListener li = new SelectionAdapter() {
      public void widgetSelected( final SelectionEvent evt ) {
        Widget widget = evt.widget;
        int newSelected = -1;
        if( widget == buttons[ 1 ] ) {
          newSelected = 0;
        } else if( widget == buttons[ 2 ] ) {
          newSelected = 1;
        } else if( widget == buttons[ 3 ] ) {
          newSelected = 2;
        }
        selected = newSelected;
        notifyListeners( new Integer( newSelected ) );
      }
    };
    for( int i = 0; i < buttons.length; i++ ) {
      buttons[ i ].addSelectionListener( li );
    }
  }

  private void createLabel() {
    Label label = new Label( this, SWT.WRAP );
    String info =   "These options specify convenient \"packages\" "
                  + "of optimization flags.";
    label.setText( info );
  }
  
  
  // helping methods
  //////////////////
  
  private void applySelection() {
    buttons[ selected + 1 ].setSelection( true );
  }
}