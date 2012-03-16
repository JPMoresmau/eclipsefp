/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;


/**
 * @author JP Moresmau
 *
 */
public class NewGtkProjectWizardPage extends WizardPage {

  /**
   * @param pageName
   */
  public NewGtkProjectWizardPage( ) {
    super( UITexts.newGtkProjectWizard_pageTitle );
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( final Composite arg0 ) {
    // TODO Auto-generated method stub

  }

}
