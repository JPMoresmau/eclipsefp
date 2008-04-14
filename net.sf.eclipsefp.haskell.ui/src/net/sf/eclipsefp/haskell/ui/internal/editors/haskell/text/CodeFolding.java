package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class CodeFolding implements ICodeFolding {

  // TODO: this implementation has been generated to comply with the
  // ICodeFolding interface. If you change the interface, you
  // will probably have to adjust this file to your changes.

  // This implementation class is mostly for marshalling parameters that
  // come from Java code into strings that can be sent over to the Haskell
  // side, where they are unmarshalled and passed to the actual Haskell
  // code. The interface ICodeFolding is used for obtaining a function
  // instance and executing the function. Typically, code on the Java
  // side that uses your Haskell function will look like this:
  //
  //    // get our function from the factory
  //    CohatoeServer server = CohatoeServer.getInstance();
  //    ICodeFolding codeFolding = server.createFunction( ICodeFolding.class );
  //    if( codeFolding != null ) {
  //      // talk to Haskell by calling it
  //      String result = codeFolding.performCodeFolding( "myParam" );
  //      TODO do sth. with result
  //    }

  public List<ICodeFoldingRegion> performCodeFolding( final String param ) {
    List<ICodeFoldingRegion> result = new ArrayList<ICodeFoldingRegion>();
    String[] params = new String[] { param };
    CohatoeServer server = CohatoeServer.getInstance();
    try {
      String[] retVal = server.evaluate( ICodeFolding.class, params );
      umarshal( retVal, result );
    } catch( CohatoeException cex ) {
      HaskellUIPlugin.log( cex );
    }
    return result;
  }


  // helping functions
  ////////////////////

  private void umarshal( final String[] retVal,
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
