// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.util;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;


/** <p>an IStatus convenience implementation.</p>
  * 
  * @author Leif Frenzel
  */
public class DefaultStatus extends Status {

  public DefaultStatus( final int statusCode ) {
    super( statusCode, HaskellUIPlugin.getPluginId(), IStatus.OK, "", null );
  }
  
  public DefaultStatus() {
    this( IStatus.OK );
  }
  
  public void setError( final String errorMessage ) {
    setMessage( errorMessage );
    setSeverity( IStatus.ERROR );
  }  
}