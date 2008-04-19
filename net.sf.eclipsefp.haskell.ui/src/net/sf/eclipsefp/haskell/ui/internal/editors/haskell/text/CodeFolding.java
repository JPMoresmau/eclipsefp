// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class CodeFolding implements ICodeFolding {

  public List<ICodeFoldingRegion> performCodeFolding( final String param ) {
    List<ICodeFoldingRegion> result = new ArrayList<ICodeFoldingRegion>();
    String[] params = new String[] { param };
    CohatoeServer server = CohatoeServer.getInstance();
    try {
      String[] retVal = server.evaluate( ICodeFolding.class, params );
      unmarshal( retVal, result );
    } catch( CohatoeException cex ) {
      HaskellUIPlugin.log( cex );
    }
    return result;
  }


  // helping functions
  ////////////////////

  private void unmarshal( final String[] retVal,
                          final List<ICodeFoldingRegion> result ) {
    if( retVal != null ) {
      int index = 0;
      while( retVal.length > index + 1 ) {
        final int start = readInt( retVal[ index++ ] );
        final int end = readInt( retVal[ index++ ] );
        result.add( new ICodeFoldingRegion() {
          public int getStartLine() {
            return start;
          }
          public int getEndLine() {
            return end;
          }
        } );
      }
    }
  }

  private int readInt( final String str ) {
    int result = 0;
    try {
      result = Integer.parseInt( str );
    } catch( final NumberFormatException numfex ) {
      // not a number...
    }
    return result;
  }
}
