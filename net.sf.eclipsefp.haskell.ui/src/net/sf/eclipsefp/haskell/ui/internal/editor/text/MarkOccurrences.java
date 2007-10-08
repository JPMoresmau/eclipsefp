// Copyright (c) 2007 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.editor.text;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class MarkOccurrences implements IMarkOccurrences {

  // interface methods of IMarkOccurrences
  ////////////////////////////////////////
  
  public Occurrence[] mark( final String bufferContent, 
                            final int cursorLine, 
                            final int cursorColumn ) {
    List<Occurrence> result = new ArrayList<Occurrence>();
    String[] params = new String[] { 
      bufferContent, 
      String.valueOf( cursorLine ), 
      String.valueOf( cursorColumn ) 
    };
    try {
      CohatoeServer server = CohatoeServer.getInstance();
      String[] retVal = server.evaluate( IMarkOccurrences.class, params );
      read( retVal, result );
    } catch( final CohatoeException cox ) {
      HaskellUIPlugin.getDefault().getLog().log( cox.getStatus() );
    }
    return result.toArray( new Occurrence[ result.size() ] );
  }

  
  // helping functions
  ////////////////////
  
  private void read( final String[] retVal, final List<Occurrence> result ) {
    if( retVal != null ) {
      int index = 0;
      while( index + 2 < retVal.length ) {
        int line = Integer.parseInt( retVal[ index++ ] );
        int column = Integer.parseInt( retVal[ index++ ] );
        int length = Integer.parseInt( retVal[ index++ ] );
        result.add( new Occurrence( line, column, length ) );
      }
    }
  }
}
