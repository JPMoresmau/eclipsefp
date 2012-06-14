/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.ArrayList;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.wizard.WizardPage;

/**
 * Manager of validators
 * @author JP Moresmau
 *
 */
public final class ValidatorManager {

  private final WizardPage fWizardPage;

  private final ArrayList<Validator> validators = new ArrayList<Validator>();

  public ValidatorManager(final WizardPage wizardPage) {
    fWizardPage = wizardPage;
  }

  public void addValidator(final Validator validator) {
    validators.add( validator );
  }

  public void updatePage() {
    String message = null;
    int status=IMessageProvider.NONE;
    boolean pageComplete = true;
    for (Validator validator : validators) {
      if (message == null && validator.getMessage() != null) {
        message = validator.getMessage();
      }
      if (validator.getStatus()>status){
        status=validator.getStatus();
        if (validator.getMessage()!=null){
          message = validator.getMessage();
        }
      }

      if (pageComplete && !validator.isPageComplete()) {
        pageComplete = false;
      }
    }
    fWizardPage.setMessage( message,status );
    fWizardPage.setPageComplete( pageComplete );
  }

  public void fullUpdate() {
    for (Validator validator : validators) {
      validator.update();
    }
    updatePage();
  }

}

