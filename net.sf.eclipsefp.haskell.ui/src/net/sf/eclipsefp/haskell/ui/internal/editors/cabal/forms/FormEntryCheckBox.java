package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class FormEntryCheckBox extends FormEntry implements SelectionListener {

  static final String TRUE = "true";
  static final String FALSE = "";

  boolean ignoreModify = true;
  String title;
  Button checkbox;

  public FormEntryCheckBox(final String title) {
    this.title = title;
  }

  @Override
  public void init( final IProject project, final Composite parent, final FormToolkit toolkit,
      final int style ) {
    checkbox = toolkit.createButton( parent, title, SWT.CHECK );
    checkbox.addSelectionListener( this );
  }

  @Override
  public Control getControl() {
    return checkbox;
  }

  @Override
  public int heightHint() {
    return 15;
  }

  @Override
  public void setValue( final String value, final boolean blockNotification ) {
    ignoreModify = blockNotification;
    checkbox.setSelection( value != null && value.equals( TRUE ) );
    ignoreModify = false;
  }

  @Override
  public String getValue() {
    return checkbox.getSelection() ? TRUE : FALSE;
  }

  @Override
  public void setEditable( final boolean editable ) {
    checkbox.setEnabled( editable );
  }

  public void widgetSelected( final SelectionEvent e ) {
    if (!ignoreModify) {
      notifyTextValueChanged();
    }
  }

  public void widgetDefaultSelected( final SelectionEvent e ) {
    // Do nothing
  }

}
