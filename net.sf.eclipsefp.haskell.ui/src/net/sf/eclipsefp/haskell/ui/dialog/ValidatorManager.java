package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.ArrayList;
import org.eclipse.jface.wizard.WizardPage;

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
    String errorMessage = null;
    boolean pageComplete = true;
    for (Validator validator : validators) {
      if (message == null && validator.getMessage() != null) {
        message = validator.getMessage();
      }
      if (errorMessage == null && validator.getErrorMessage() != null) {
        errorMessage = validator.getErrorMessage();
      }
      if (pageComplete && !validator.isPageComplete()) {
        pageComplete = false;
      }
    }
    fWizardPage.setMessage( message );
    fWizardPage.setErrorMessage( errorMessage );
    fWizardPage.setPageComplete( pageComplete );
  }

  public void fullUpdate() {
    for (Validator validator : validators) {
      validator.update();
    }
    updatePage();
  }

}

