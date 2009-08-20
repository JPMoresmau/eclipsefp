package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.ArrayList;
import org.eclipse.jface.wizard.WizardPage;

/**
 * Manages multiple {@link Validator}s for a wizard page.
 * This aggregates the states from all validators, sets the page's
 * completion state, and displays the appropriate message on the page.
 *
 * The page is complete if all validators indicate completion.
 * If some validator indicates an error message, this message is shown.
 * If not, but some validator indicates a (non-error) message, this message is shown.
 * Otherwise, no message is shown.
 */
public final class ValidatorManager {

  private final WizardPage fWizardPage;

  private final ArrayList<Validator> validators = new ArrayList<Validator>();

  public ValidatorManager(final WizardPage wizardPage) {
    fWizardPage = wizardPage;
  }

  public void addValidator(final Validator validator) {
    if (!validators.contains( validator )) {
      validators.add( validator );
      validator.setManager( this );
    }
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

