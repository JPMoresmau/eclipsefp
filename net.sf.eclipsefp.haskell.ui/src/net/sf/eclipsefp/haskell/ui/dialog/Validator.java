/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.Observable;
import java.util.Observer;
import org.eclipse.jface.dialogs.IMessageProvider;

/**
 * validation status
 * @author JP Moresmau
 *
 */
public abstract class Validator implements Observer {

  private ValidatorManager fManager;

  private String fMessage;
  private int status=IMessageProvider.NONE;
  private boolean fPageComplete=false;

  public Validator() {
    fManager = null;
  }

  public Validator( final ValidatorManager manager ) {
    setManager( manager );
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
    status=IMessageProvider.NONE;
  }

  public void setErrorMessage( final String message ) {
    if (message!=null){
      fMessage = message;
      status=IMessageProvider.ERROR;
    }
  }

  public void setWarningMessage( final String message ) {
    if (message!=null){
      fMessage = message;
      status=IMessageProvider.WARNING;
    }
  }

  public boolean isPageComplete() {
    return fPageComplete;
  }

  public void setPageComplete( final boolean pageComplete ) {
    fPageComplete = pageComplete;
  }

  public void setComplete() {
    setMessage( null );
    setStatus( IMessageProvider.NONE );
    setPageComplete( true );
  }


  /**
   * @return the status
   */
  public int getStatus() {
    return status;
  }

  /**
   * @param status the status to set
   */
  public void setStatus( final int status ) {
    this.status = status;
  }

  public void setIncomplete( final String message){
    setIncomplete( message, IMessageProvider.ERROR );
  }

  public void setIncomplete( final String message, final int status ) {

    setMessage( message );
    setStatus( status );
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
    fManager.updatePage();
  }

  // //////////////////////////
  // methods from Observable

  @Override
  public void update( final Observable o, final Object arg ) {
    update();
  }

}
