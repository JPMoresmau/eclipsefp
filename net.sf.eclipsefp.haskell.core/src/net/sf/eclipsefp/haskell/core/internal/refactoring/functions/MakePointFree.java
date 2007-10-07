// Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.internal.refactoring.functions;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** <p>implementation class to access the Haskell implementation of the
  * pointfree refactoring.</p> 
  *
  * @author Leif Frenzel
  */
public class MakePointFree implements IMakePointFree {

  public String makePointFree( final String content ) {
    String result = null;
    try {
      String[] params = new String[] { content };
      CohatoeServer server = CohatoeServer.getInstance();
      String[] retVal = server.evaluate( IMakePointFree.class, params );
      if( retVal != null && retVal.length == 1 ) {
        result = retVal[ 0 ];
      }
    } catch( final CohatoeException cohex ) {
      // we don't handle this here, just write it to the workspace log
      HaskellCorePlugin.getDefault().getLog().log( cohex.getStatus() );
    }
    return result;
  }
}
