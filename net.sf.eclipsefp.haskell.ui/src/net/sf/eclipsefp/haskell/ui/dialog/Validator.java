package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.Observable;
import java.util.Observer;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.DialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.IDialogFieldListener;

/**
 * Validates input in a wizard.
 * This can be hooked up as a listener to dialog fields or observables.
 * Subclasses can then implement validation logic and call the appropriate
 * methods to set the validation state. The {@link ValidatorManager} will
 * then take care of setting the wizard page's state.
 */
public abstract class Validator implements Observer, IDialogFieldListener {

  private ValidatorManager fManager;

  private String fMessage;
  private String fErrorMessage;
  private boolean fPageComplete;

  public Validator() {
    fManager = null;
  }

  public Validator( final ValidatorManager manager ) {
    setManager( manager );
  }

  public ValidatorManager getManager() {
    return fManager;
  }

  public void setManager( final ValidatorManager manager ) {
    if( fManager == null ) {
      fManager = manager;
      manager.addValidator( this );
    }
  }

  public String getMessage() {
    return fMessage;
  }

  public void setMessage( final String message ) {
    fMessage = message;
  }

  public String getErrorMessage() {
    return fErrorMessage;
  }

  public void setErrorMessage( final String errorMessage ) {
    fErrorMessage = errorMessage;
  }

  public boolean isPageComplete() {
    return fPageComplete;
  }

  public void setPageComplete( final boolean pageComplete ) {
    fPageComplete = pageComplete;
  }

  public void setComplete() {
    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );
  }

  public void setIncomplete( final String message, final boolean error ) {
    if( error ) {
      setMessage( null );
      setErrorMessage( message );
    } else {
      setMessage( message );
      setErrorMessage( null );
    }
    setPageComplete( false );
  }

  /**
   * The only method that subclasses are required to implement.
   * On entry, message and errorMessage are null, and pageComplete is true.
   * Do validation and set state accordingly.
   */
  protected abstract void doUpdate();

  /**
   * Do not override. Rather, override {@link #doUpdate}.
   */
  public void update() {
    setComplete();
    doUpdate();
    if (fManager != null) {
      fManager.updatePage();
    }
  }

  // /////////////////////////
  // methods from Observable

  public void update( final Observable o, final Object arg ) {
    update();
  }

  // ///////////////////////////////////
  // methods from IDialogFieldListener

  public void dialogFieldChanged( final DialogField field ) {
    update();
  }

}
