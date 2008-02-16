// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;
import net.sf.eclipsefp.haskell.core.compiler.CompilerOutputItem;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;

/** <p>Breaks the ghc output into items that can be displayed in the
  * Eclipse UI.</p>
  *
  * @author Leif Frenzel
  */
public class GhcOutputParser {

  public static List<ICompilerOutputItem> parse( final String output ) {
    List<ICompilerOutputItem> results = new ArrayList<ICompilerOutputItem>();
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
        results.add( newItem );
      }
    }
    return results;
  }


  // helping methods
  //////////////////

  private static CompilerOutputItem getNewItem( final String line ) {
    CompilerOutputItem result = null;
    int index = line.indexOf( ":" ); //$NON-NLS-1$
    if( index != -1 ) {
      String fileName = line.substring( 0, index );
      int secondIndex = line.indexOf( ":", index + 1 ); //$NON-NLS-1$
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
          result = new CompilerOutputItem( fileName, unspec, "" ); //$NON-NLS-1$
        }
      }
    }
    return result;
  }

  private static boolean hasHsExtension( final String fileName ) {
    return    fileName.endsWith( ".hs" ) //$NON-NLS-1$
           || fileName.endsWith( ".lhs" ); //$NON-NLS-1$
  }

  private static String[] getLines( final String output ) {
    Vector<String> vecLines = new Vector<String>();
    StringTokenizer tokenizer = new StringTokenizer( output, "\r\n", false ); //$NON-NLS-1$
    while( tokenizer.hasMoreTokens() ) {
      vecLines.add( tokenizer.nextToken().trim() );
    }
    String[] result = new String[ vecLines.size() ];
    vecLines.toArray( result );
    return result;
  }
}