// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.refactoring.functions;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class Rename implements IRename {

  public String performRename( final String content ) {
    String result = null;
    try {
      String[] params = new String[] { content };
      CohatoeServer server = CohatoeServer.getInstance();
      String[] retVal = server.evaluate( IRename.class, params );
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
