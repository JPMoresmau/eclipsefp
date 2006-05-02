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

import org.eclipse.jface.util.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.*;

/**
 * Dialog field containing a label, text control and a button control.
 */
public class StringButtonDialogField extends StringDialogField {

  private Button fBrowseButton;

  private String fBrowseButtonLabel;

  private IStringButtonAdapter fStringButtonAdapter;

  private boolean fButtonEnabled;

  public StringButtonDialogField(IStringButtonAdapter adapter) {
    super();
    fStringButtonAdapter = adapter;
    fBrowseButtonLabel = "!Browse...!"; //$NON-NLS-1$
    fButtonEnabled = true;
  }

  /**
   * Sets the label of the button.
   */
  public void setButtonLabel(String label) {
    fBrowseButtonLabel = label;
  }

  // ------ adapter communication

  /**
   * Programmatical pressing of the button
   */
  public void changeControlPressed() {
    fStringButtonAdapter.changeControlPressed(this);
  }

  // ------- layout helpers

  /*
   * @see DialogField#doFillIntoGrid
   */
  public Control[] doFillIntoGrid(Composite parent, int nColumns) {
    Assert.isTrue(nColumns >= getNumberOfControls(),
        "given number of columns is too small");

    Label label = getLabelControl(parent);
    label.setLayoutData(gridDataForLabel(1));
    Text text = getTextControl(parent);
    text.setLayoutData(gridDataForText(nColumns - 2));
    Button button = getChangeControl(parent);
    button.setLayoutData(gridDataForButton( 1));

    return new Control[] { label, text, button };
  }

  public int getNumberOfControls() {
    return 3;
  }

  protected static GridData gridDataForButton( final int span ) {
    GridData result = new GridData();
    result.horizontalAlignment = GridData.FILL;
    result.grabExcessHorizontalSpace = false;
    result.horizontalSpan = span;
    return result;
  }

  // ------- ui creation

  /**
   * Creates or returns the created buttom widget.
   * 
   * @param parent
   *          The parent composite or <code>null</code> if the widget has
   *          already been created.
   */
  public Button getChangeControl(Composite parent) {
    if (fBrowseButton == null) {
      Assert.isNotNull(parent,
          "uncreated control requested with composite null");

      fBrowseButton = new Button(parent, SWT.PUSH);
      fBrowseButton.setText(fBrowseButtonLabel);
      fBrowseButton.setEnabled(isEnabled() && fButtonEnabled);
      fBrowseButton.addSelectionListener(new SelectionListener() {
        public void widgetDefaultSelected(SelectionEvent e) {
          changeControlPressed();
        }

        public void widgetSelected(SelectionEvent e) {
          changeControlPressed();
        }
      });

    }
    return fBrowseButton;
  }

  // ------ enable / disable management

  /**
   * Sets the enable state of the button.
   */
  public void enableButton(boolean enable) {
    if (isOkToUse(fBrowseButton)) {
      fBrowseButton.setEnabled(isEnabled() && enable);
    }
    fButtonEnabled = enable;
  }

  /*
   * @see DialogField#updateEnableState
   */
  protected void updateEnableState() {
    super.updateEnableState();
    if (isOkToUse(fBrowseButton)) {
      fBrowseButton.setEnabled(isEnabled() && fButtonEnabled);
    }
  }
}