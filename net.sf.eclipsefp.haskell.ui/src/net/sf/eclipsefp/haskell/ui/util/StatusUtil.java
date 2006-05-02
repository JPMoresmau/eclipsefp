// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IMessageProvider;


/** contains utility functionality related to status analysis and displaying.
  * 
  * @author Leif Frenzel
  */
public class StatusUtil {

  public static IStatus getMostSevere( final IStatus[] status ) {
    IStatus result = new DefaultStatus();
    boolean found = false;
    for( int i = 0; !found && i < status.length; i++ ) {
      IStatus curr = status[ i ];
      if( result == null || curr.getSeverity() > result.getSeverity() ) {
        result = curr;
      }
      if( curr.matches( IStatus.ERROR ) ) {
        result = curr;
        found = true;
      }
    }
    return result;
  }

  /**
   * Applies the status to the status line of a dialog page.
   */
  public static void applyToStatusLine( final DialogPage page, 
                                        final IStatus status ) {
    String message = status.getMessage();
    if( message.equals( "" ) ) {
      message = null;
    }
    switch( status.getSeverity() ) {
      case IStatus.OK:
        page.setMessage( message, IMessageProvider.NONE );
        page.setErrorMessage( null );
        break;
      case IStatus.WARNING:
        page.setMessage( message, IMessageProvider.WARNING );
        page.setErrorMessage( null );
        break;
      case IStatus.INFO:
        page.setMessage( message, IMessageProvider.INFORMATION );
        page.setErrorMessage( null );
        break;
      default:
        if( message != null && message.length() == 0 ) {
          message = null;
        }
        page.setMessage( null );
        page.setErrorMessage( message );
        break;
    }
  }
}