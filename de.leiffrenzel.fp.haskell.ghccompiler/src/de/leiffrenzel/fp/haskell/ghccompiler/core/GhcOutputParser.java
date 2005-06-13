// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.core;

import java.util.StringTokenizer;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.compiler.*;

/** <p>Breaks the ghc output into items that can be displayed in the 
  * Eclipse UI.</p>
  * 
  * @author Leif Frenzel
  */
public class GhcOutputParser implements IHaskellCompilerOutputParser {

  public ICompilerOutputItem[] getItems( final ICompilerOutput output ) {
    return parse( output.getOutput() + output.getErrors() );
  }

  private ICompilerOutputItem[] parse( final String output ) {
    Vector vecResults = new Vector();
    CompilerOutputItem lastItem = null;
    String[] lines = getLines( output );
    for( int i = 0; i < lines.length; i++ ) {
      CompilerOutputItem newItem = getNewItem( lines[ i ] );
      if( newItem == null ) { 
        if( lastItem != null ) {
          lastItem.addToComment( lines[ i ] );
        }
      } else {
        lastItem = newItem;
        vecResults.add( newItem );
      } 
    }
    return createArray( vecResults );
  }
  

  // helping methods
  //////////////////

  private static CompilerOutputItem getNewItem( final String line ) {
    CompilerOutputItem result = null;
    int index = line.indexOf( ":" );
    if( index != -1 ) {
      String fileName = line.substring( 0, index );
      int secondIndex = line.indexOf( ":", index + 1 );
      int lineNum = -1;
      if( hasHsExtension( fileName ) ) {
        if( secondIndex != -1 ) {
          String sLineNum = line.substring( index + 1, secondIndex );
          try {
            lineNum = Integer.parseInt( sLineNum );
          } catch( Exception ignored ) {
            lineNum = -1;
          }
          if( lineNum != -1 ) {
            String rest = line.substring( secondIndex + 1 );
            result = new CompilerOutputItem( fileName, lineNum, rest );
          }
        } else {
          int unspec = ICompilerOutputItem.LINE_UNSPECIFIED;
          result = new CompilerOutputItem( fileName, unspec, "" );
        }
      }      
    }
    return result;
  }

  private static boolean hasHsExtension( final String fileName ) {
    return    fileName.endsWith( ".hs" )
           || fileName.endsWith( ".lhs" );
  }

  private static String[] getLines( final String output ) {
    Vector vecLines = new Vector();
    StringTokenizer tokenizer = new StringTokenizer( output, "\r\n", false );
    while( tokenizer.hasMoreTokens() ) {
      vecLines.add( tokenizer.nextToken().trim() );
    }
    String[] result = new String[ vecLines.size() ];
    vecLines.toArray( result );
    return result;
  }

  private static ICompilerOutputItem[] createArray( final Vector vec ) {
    ICompilerOutputItem[] result = new CompilerOutputItem[ vec.size() ];
    vec.toArray( result );
    return result;
  }
}