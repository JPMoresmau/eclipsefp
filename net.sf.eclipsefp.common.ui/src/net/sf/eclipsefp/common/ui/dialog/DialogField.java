// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;



/** <p>the superclass for all dialog fields.</p>
  * 
  * @author Leif Frenzel
  */
public abstract class DialogField extends Composite {

  private final List<IDialogFieldListener> listeners;
  
  public DialogField( final Composite parent ) {
    super( parent, SWT.NONE );
    listeners = new ArrayList<IDialogFieldListener>();
  }
  
  
  // helping functionality for subclasses
  ///////////////////////////////////////
  
  protected void notifyListeners( final Object newInfo ) {
    for( int i = 0; i < listeners.size(); i++ ) {
      listeners.get( i ).infoChanged( newInfo );
    }
  }
  
  
  // attribute setters and getters
  ////////////////////////////////
  
  public abstract void setInfo( final Object info );
  public abstract Object getInfo();

  public void addDialogFieldListener( final IDialogFieldListener listener ) {
    listeners.add( listener );
  }
}