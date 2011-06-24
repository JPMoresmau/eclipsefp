/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CCombo;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class FormEntryCombo<T> extends FormEntry {

  private CCombo comboField;
  private final Choice<T> choices;
  private String value = ""; //$NON-NLS-1$
  private boolean dirty;
  private boolean isIgnoreModify = false;

  public FormEntryCombo( final Choice<T> choices ) {
    this.choices = choices;
  }

  @Override
  public void init( final IProject project, final Composite parent,
      final FormToolkit toolkit, final int style ) {
    int finalStyle = style | SWT.FLAT;
    if (!choices.allowOther()) {
      finalStyle |= SWT.READ_ONLY;
    }
    this.comboField = new CCombo( parent, finalStyle );
    toolkit.adapt( comboField, true, true );
    comboField.setItems( choices.getAllShownStrings() );
    addListeners();
  }

  @Override
  public Control getControl() {
    return this.comboField;
  }

  @Override
  public int heightHint() {
    return 20;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    this.isIgnoreModify = blockNotification;
    // Find the element to show
    if (value != null) {
      T item = choices.fromCabalString( value );
      if (item != null) {
        this.comboField.setText( choices.toShownString( item ) );
      } else {
        this.comboField.setText( value );
      }
    } else {
      this.comboField.setText( "" );
    }
    // Save the real Cabal value
    this.value = ( value != null ) ? value : ""; //$NON-NLS-1$
    this.isIgnoreModify = false;
  }

  @Override
  public String getValue() {
    return value.trim();
  }

  @Override
  public void setEditable( final boolean editable ) {
    this.comboField.setEnabled( editable );
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
      String text = comboField.getText();
      T item = choices.fromShownString( text );
      if (item != null) {
        value = choices.toCabalString( item );
      } else {
        value = comboField.getText();
      }
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
    if( evt.character == '\r' && ( comboField.getStyle() & SWT.MULTI ) == 0 ) {
      // commit value
      if( dirty ) {
        commit();
      }
    } else if( evt.character == '\u001b' ) { // Escape character
      String shown;
      if (value != null) {
        T item = choices.fromCabalString( value );
        if (item != null) {
          shown = choices.toShownString( item );
        } else {
          shown = value;
        }
      } else {
        shown = "";
      }

      if( !shown.equals( comboField.getText() ) ) {
        comboField.setText( shown ); // restore old
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
    comboField.addKeyListener( new KeyAdapter() {

      @Override
      public void keyReleased( final KeyEvent evt ) {
        keyReleaseOccured( evt );
      }
    } );
    comboField.addModifyListener( new ModifyListener() {

      public void modifyText( final ModifyEvent evt ) {
        if( !comboField.getText().equals( value ) ) {
          editOccured( evt );
        }
      }
    } );
    comboField.addFocusListener( new FocusAdapter() {

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
