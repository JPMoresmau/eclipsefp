// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;


/** <p>listens for changes on dialog fields.</p>
  * 
  * @author Leif Frenzel
  */
public interface IDialogFieldListener {

  /** <p>notified when the content of the dialog field, where this listener
    * is registered, has changed.</p> */
  void infoChanged( final Object newInfo );
}