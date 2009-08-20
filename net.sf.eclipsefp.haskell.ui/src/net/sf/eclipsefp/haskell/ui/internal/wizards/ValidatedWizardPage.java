package net.sf.eclipsefp.haskell.ui.internal.wizards;


import net.sf.eclipsefp.haskell.ui.dialog.ValidatorManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * A wizard page with validators attached.
 * Implementors must override {@link #doCreateControl} and
 * may override {@link #createValidators}.
 */
public abstract class ValidatedWizardPage extends WizardPage {

  protected final ValidatorManager fValidatorManager;

  public ValidatedWizardPage( final String pageName ) {
    this( pageName, null, null );
  }

  public ValidatedWizardPage( final String pageName, final String title,
      final ImageDescriptor titleImage ) {
    super( pageName, title, titleImage );
    fValidatorManager= new ValidatorManager(this);
  }

  /**
   * Subclasses should override to add validators,
   * but must call the inherited method.
   *
   * At the time this is called, {@link #doCreateControl} has already run,
   * so implementors can assume that all controls have been created.
   *
   * Subclasses must not call this method.
   */
  protected abstract void createValidators( final ValidatorManager manager );

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   *
   * Subclasses should not override; rather, override {@link #doCreateControl}.
   */
  public void createControl(final Composite parent) {
    initializeDialogUnits(parent);

    Control control = doCreateControl(parent);
    setControl(control);

    createValidators(fValidatorManager);
    fValidatorManager.fullUpdate();
  }

  /**
   * Subclasses should override this to create and return the actual control.
   * There is no need to call {@link #setControl(Control)} from overriding methods.
   *
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  protected abstract Control doCreateControl(final Composite parent);

}