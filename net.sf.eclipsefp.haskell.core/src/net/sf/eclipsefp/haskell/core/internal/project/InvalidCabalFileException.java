// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

/** <p>exception to indicate a Cabal file that could not be read.</p>
  *
  * @author Leif Frenzel
  */
public class InvalidCabalFileException extends CoreException {

  private static final long serialVersionUID = 1L;

  InvalidCabalFileException( final String msg ) {
    super( createStatus( msg ) );
  }

  private static final IStatus createStatus( final String message ) {
    String msg = message == null ? CoreTexts.invalidCabalFileException_noDetail
                                 : message;
    String pluginId = HaskellCorePlugin.getPluginId();
    return new Status( IStatus.ERROR, pluginId, 0, msg, null );
  }
}
