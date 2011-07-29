/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
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
  private String value = ""; //$NON-NLS-1$
  private boolean dirty;
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

  @Override
  public int heightHint() {
    // Make more space for multiline editors
    return ( this.textField.getStyle() & SWT.MULTI ) > 0 ? 45 : 15;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    this.isIgnoreModify = blockNotification;
    this.textField.setText( value != null ? value : "" ); //$NON-NLS-1$
    this.value = ( value != null ) ? value : ""; //$NON-NLS-1$
    this.isIgnoreModify = false;
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
    return dirty;
  }

  /**
   * If dirty, commits the text in the widget to the value and notifies the
   * listener. This call clears the 'dirty' flag.
   */
  public void commit() {
    if( dirty ) {
      value = textField.getText();
      notifyTextValueChanged();
    }
    dirty = false;
  }

  public void cancelEdit() {
    dirty = false;
  }

  // helping functions
  // //////////////////

  private void keyReleaseOccured( final KeyEvent evt ) {
    if( evt.character == '\r' && ( textField.getStyle() & SWT.MULTI ) == 0 ) {
      // commit value
      if( dirty ) {
        commit();
      }
    } else if( evt.character == '\u001b' ) { // Escape character
      if( !value.equals( textField.getText() ) ) {
        textField.setText( value != null ? value : "" ); // restore old //$NON-NLS-1$
      }
      dirty = false;
    }
    notifySelectionChanged();
  }

  private void editOccured( final ModifyEvent evt ) {
    if( !isIgnoreModify ) {
      dirty = true;
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
        if( dirty ) {
          commit();
        }
      }
    } );
  }
}
