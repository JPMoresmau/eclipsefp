// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.code;

import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class OrganizeImports implements IOrganizeImports {

  // interface methods of IOrganizeImports
  ////////////////////////////////////////

  public String organizeImports( final String buffer ) throws CohatoeException {
    String result = null;
    String[] params = new String[] { buffer };
    CohatoeServer server = CohatoeServer.getInstance();
    String[] retVal = server.evaluate( IOrganizeImports.class, params );

    if( retVal != null && retVal.length == 1 ) {
      result = retVal[ 0 ];
    }
    return result;
  }
}
