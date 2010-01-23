/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui.dialog.dialogfields;

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * Dialog field containing a label and a text control.
 */
public class StringDialogField extends DialogField {

  private String text;
  private Text txtControl;

  private ModifyListener fModifyListener;

  public StringDialogField() {
    super();
    text = ""; //$NON-NLS-1$
  }

  // ------- layout helpers

  /*
   * @see DialogField#doFillIntoGrid
   */
  @Override
  public Control[] doFillIntoGrid(final Composite parent, final int nColumns) {
    Assert.isTrue(nColumns >= getNumberOfControls(),
        "given number of columns is too small"); //$NON-NLS-1$

    Label label = getLabelControl(parent);
    label.setLayoutData(gridDataForLabel(1));
    Text text = getTextControl(parent);
    text.setLayoutData(gridDataForText(nColumns - 1));

    return new Control[] { label, text };
  }

  /*
   * @see DialogField#getNumberOfControls
   */
  @Override
  public int getNumberOfControls() {
    return 2;
  }

  protected static GridData gridDataForText( final int span ) {
    GridData gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = false;
    gd.horizontalSpan = span;
    return gd;
  }

  // ------- focus methods

  /*
   * @see DialogField#setFocus
   */
  @Override
  public boolean setFocus() {
    if (isOkToUse(txtControl)) {
      txtControl.setFocus();
      txtControl.setSelection(0, txtControl.getText().length());
    }
    return true;
  }

  // ------- ui creation

  /**
   * Creates or returns the created text control.
   *
   * @param parent
   *          The parent composite or <code>null</code> when the widget has
   *          already been created.
   */
  public Text getTextControl(final Composite parent) {
    if (txtControl == null) {
      Assert.isNotNull(parent,
          "uncreated control requested with composite null"); //$NON-NLS-1$
      fModifyListener = new ModifyListener() {
        public void modifyText( final ModifyEvent event ) {
          doModifyText();
        }
      };

      txtControl = new Text(parent, SWT.SINGLE | SWT.BORDER);
      // moved up due to 1GEUNW2
      txtControl.setText(text);
      txtControl.setFont(parent.getFont());
      txtControl.addModifyListener(fModifyListener);

      txtControl.setEnabled(isEnabled());
    }
    return txtControl;
  }

  private void doModifyText() {
    if (isOkToUse(txtControl)) {
      text = txtControl.getText();
    }
    dialogFieldChanged();
  }

  // ------ enable / disable management

  /*
   * @see DialogField#updateEnableState
   */
  @Override
  protected void updateEnableState() {
    super.updateEnableState();
    if (isOkToUse(txtControl)) {
      txtControl.setEnabled(isEnabled());
    }
  }

  // ------ text access

  /**
   * Gets the text. Can not be <code>null</code>
   */
  public String getText() {
    return text;
  }

  /**
   * Sets the text. Triggers a dialog-changed event.
   */
  public void setText( final String text ) {
    this.text = text;
    if (isOkToUse(txtControl)) {
      txtControl.setText(text);
    } else {
      dialogFieldChanged();
    }
  }

  /**
   * Sets the text without triggering a dialog-changed event.
   */
  public void setTextWithoutUpdate( final String text ) {
    this.text = text;
    if (isOkToUse(txtControl)) {
      txtControl.removeModifyListener(fModifyListener);
      txtControl.setText(text);
      txtControl.addModifyListener(fModifyListener);
    }
  }
}
