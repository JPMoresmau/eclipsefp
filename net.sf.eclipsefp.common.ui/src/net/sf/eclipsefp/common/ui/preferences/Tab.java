// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences;

import java.util.*;

import net.sf.eclipsefp.common.ui.util.DialogUtil;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.*;




/** <p>the super class for all tabs on a tabbed preference page.</p>
  *
  * @author Leif Frenzel  
  */
public abstract class Tab {

  private IPreferenceStore preferenceStore;
  private Map checkBoxes = new HashMap();
  private Map labels = new HashMap();
  private Map textFields = new HashMap();
  
  private SelectionListener fCheckBoxListener = new SelectionAdapter() {
    public void widgetSelected( final SelectionEvent event ) {
      Button button = ( Button )event.widget;
      String key = ( String )checkBoxes.get( button );
      getPreferenceStore().setValue( key, button.getSelection() );
    }
  };

  private ModifyListener fTextFieldListener = new ModifyListener() {
    public void modifyText( final ModifyEvent event ) {
      Text text = ( Text )event.widget;
      String key = ( String )textFields.get( text );
      getPreferenceStore().setValue( key, text.getText() );
    }
  };
  
  public Tab( final IPreferenceStore store ) {
    this.preferenceStore = store;
  }

  public abstract Control createControl( Composite parent );
  
  public void initializeFields() {
    initializeCheckboxes();
    initializeTexts();
  }

  protected Control getLabel( final Control field ) {
    return ( Control )labels.get( field );
  }

  protected IPreferenceStore getPreferenceStore() {
    return preferenceStore;
  }

  
  // UI creation helping methods for subclasses
  /////////////////////////////////////////////
  
  protected void createLabel( final Composite parent, final String text ) {
    Label label = new Label( parent, SWT.LEFT );
    label.setText( text );
    GridData gridData = new GridData();
    gridData.horizontalAlignment = GridData.BEGINNING;
    label.setLayoutData( gridData );
  }
  
  protected final Button addBooleanField( final Composite parent, 
                                          final String label,
                                          final String key, 
                                          final int indentation ) {
    Button checkBox = new Button( parent, SWT.CHECK );
    checkBox.setText( label );

    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.horizontalIndent = indentation;
    gd.horizontalSpan = 2;
    checkBox.setLayoutData( gd );
    checkBox.addSelectionListener( fCheckBoxListener );

    checkBoxes.put( checkBox, key );

    return checkBox;
  }

  protected Control addIntegerField( final Composite composite, 
                                     final String label,
                                     final String key, 
                                     final int textLimit, 
                                     final int indentation ) {
    Text result = addStringField( composite, label, textLimit, indentation );
    textFields.put( result, key );
    return result;
  }

  protected Control addTextField( final Composite composite, 
                                  final String label,
                                  final String key, 
                                  final int textLimit, 
                                  final int indentation ) {
    Text result = addStringField( composite, label, textLimit, indentation );
    textFields.put( result, key );
    result.addModifyListener( fTextFieldListener );
    return result;
  }
  
  // helping methods
  //////////////////

  private Text addStringField( final Composite composite, 
                               final String label,
                               final int textLimit, 
                               final int indentation ) {
    Label labelControl = new Label( composite, SWT.NONE );
    labelControl.setText( label );
    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.horizontalIndent = indentation;
    labelControl.setLayoutData( gd );

    Text textControl = new Text( composite, SWT.BORDER | SWT.SINGLE );
    gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.widthHint = DialogUtil.convertWidthInCharsToPixels( textControl, 
                                                           textLimit + 1 );
    textControl.setLayoutData( gd );
    textControl.setTextLimit( textLimit );

    labels.put( textControl, labelControl );
    return textControl;
  }
  
  private void initializeTexts() {
    Iterator iter = textFields.keySet().iterator();
    while( iter.hasNext() ) {
      Text text = ( Text )iter.next();
      String key = ( String )textFields.get( text );
      text.setText( getPreferenceStore().getString( key ) );
    }
  }

  private void initializeCheckboxes() {
    Iterator iter = checkBoxes.keySet().iterator();
    while( iter.hasNext() ) {
      Button button = ( Button )iter.next();
      String key = ( String )checkBoxes.get( button );
      button.setSelection( getPreferenceStore().getBoolean( key ) );
    }
  }
}