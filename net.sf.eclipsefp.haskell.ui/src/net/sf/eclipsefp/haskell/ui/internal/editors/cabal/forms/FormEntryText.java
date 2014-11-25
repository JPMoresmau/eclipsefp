/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * copied from PDE UI internals and modified
 *
 * The helper class for creating entry fields with label and text. Optionally, a
 * button can be added after the text. The attached listener reacts to all the
 * events. Entring new text makes the entry 'dirty', but only when 'commit' is
 * called is 'valueChanged' method called (and only if 'dirty' flag is set).
 * This allows delayed commit.
 *
 * Based on previous work of Leif Frenzel
 */
public class FormEntryText extends FormEntry {

  private Text textField;
  private String oldValue = null;
  private String value = ""; //$NON-NLS-1$
  private boolean isIgnoreModify = false;

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    this.textField = toolkit.createText( parent, "", style );
    addListeners();
  }

  @Override
  public Control getControl() {
    return this.textField;
  }

  private boolean isMultiline(){
    return  ( this.textField.getStyle() & SWT.MULTI ) > 0;
  }

  @Override
  public int heightHint() {
    // Make more space for multiline editors
    return isMultiline() ? 45 : 15;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    // we don't modify while we're editing
    if (this.textField.isFocusControl()){
      return;
    }
    this.isIgnoreModify = blockNotification;
    // remove line separators if we're not multiline
    this.value = ( value != null ) ? isMultiline()?
        value
        : value.replace( "\n", " " ).replace( "\r", "" )
        : ""; //$NON-NLS-1$
    String v=this.value != null ? this.value : "" ;//$NON-NLS-1$
    if (!v.equals(this.textField.getText())){
      this.textField.setText( v );
    }


    this.isIgnoreModify = false;
    if (oldValue==null && !blockNotification){
      oldValue=value;
    }
  }

  @Override
  public String getValue() {
    return value.trim();
  }

  @Override
  public void setEditable( final boolean editable ) {
    this.textField.setEditable( editable );
  }

  /** Returns true if the text has been modified. */
  public boolean isDirty() {
    return !value.equals(oldValue);
  }

  /**
   * If dirty, commits the text in the widget to the value and notifies the
   * listener. This call clears the 'dirty' flag.
   */
  public void commit() {
    if( isDirty() ) {
      oldValue = value;
    }
  }

  public void cancelEdit() {
    value = oldValue;
    notifyTextValueChanged();
  }

  // helping functions
  // //////////////////

  private void keyReleaseOccured( final KeyEvent evt ) {
    if( evt.character == '\r' && ( textField.getStyle() & SWT.MULTI ) == 0 ) {
      // commit value
      if( isDirty() ) {
        commit();
      }
    } else if( evt.character == '\u001b' ) { // Escape character
      //if( !textField.getText().equals( oldValue ) ) {
      int pos=this.textField.getCaretPosition();
      String v=oldValue != null ? oldValue : "" ;//$NON-NLS-1$
      textField.setText( v); // restore old
      pos=Math.min( pos, v.length() );
      this.textField.setSelection( pos, pos );
      //}
      cancelEdit();
    } else {
      value=textField.getText();
      notifyTextValueChanged();
    }
    notifySelectionChanged();
  }

  private void editOccured( final ModifyEvent evt ) {
    if( !isIgnoreModify ) {
      notifyTextDirty();
    }
  }

  private void addListeners() {
    textField.addKeyListener( new KeyAdapter() {

      @Override
      public void keyReleased( final KeyEvent evt ) {
        keyReleaseOccured( evt );
      }
    } );
    textField.addModifyListener( new ModifyListener() {

      @Override
      public void modifyText( final ModifyEvent evt ) {
        if( !textField.getText().equals( value ) ) {
          editOccured( evt );
        }
      }
    } );
    textField.addFocusListener( new FocusAdapter() {

      @Override
      public void focusGained( final FocusEvent evt ) {
        notifyFocusGained();
      }

      @Override
      public void focusLost( final FocusEvent evt ) {
        if( isDirty() ) {
          commit();
        }
      }
    } );
  }
}
