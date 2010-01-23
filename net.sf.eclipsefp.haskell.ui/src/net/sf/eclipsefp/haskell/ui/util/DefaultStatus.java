// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;


/** <p>an IStatus convenience implementation.</p>
  *
  * @author Leif Frenzel
  */
public class DefaultStatus extends Status {

  public DefaultStatus( final int statusCode ) {
    super( statusCode, HaskellUIPlugin.getPluginId(), IStatus.OK, "", null ); //$NON-NLS-1$
  }

  public DefaultStatus() {
    this( IStatus.OK );
  }

  public void setError( final String errorMessage ) {
    setMessage( errorMessage );
    setSeverity( IStatus.ERROR );
  }
}