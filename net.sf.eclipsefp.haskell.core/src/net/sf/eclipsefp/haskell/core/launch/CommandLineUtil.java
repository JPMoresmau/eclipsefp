// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.launch;

import java.util.ArrayList;


/** <p>parses command line arguments out of one big string into
  * a list.</p>
  * 
  * @author Leif Frenzel
  */
public class CommandLineUtil {

  private static final char ARG_DELIMITER = ' ';
  private static final char ARG_DBL_QUOTE = '"';

  /** <p>renders the passed array into one command line string.</p> */
  public static String renderCommandLine( final String[] commandLine ) {
    StringBuffer buf = new StringBuffer();
    if( commandLine.length > 0 ) {
      buf.append( commandLine[ 0 ] );
      for( int i = 1; i < commandLine.length; i++ ) {
        buf.append( ' ' );
        buf.append( commandLine[ i ] );
      }
    } 
    return buf.toString();
  }
  
  
  /** <p>parses the argument text into an array of individual arguments using 
    * the space character as the delimiter.</p>
    * 
    * <p>An individual argument containing spaces must have a double quote (") 
    * at the start and end. Two double quotes together is taken to mean an 
    * embedded double quote in the argument text.</p>
    */
  static String[] parse( final String content ) {
    String[] result = new String[ 0 ]; 
    if( content != null && content.length() > 0 ) {
      ArrayList alResult = new ArrayList();
      boolean inQuotes = false;
      int start = 0;
      int end = content.length();
      StringBuffer buffer = new StringBuffer( end );
      
      while( start < end ) {
        char ch = content.charAt( start );
        start++;
        
        switch( ch ) {
          case ARG_DELIMITER :
            if( inQuotes ) {
              buffer.append( ch );
            } else {
              if( buffer.length() > 0 ) {
                alResult.add( buffer.toString() );
                buffer.setLength( 0 );
              }
            }
            break;
      
          case ARG_DBL_QUOTE :
            if( start < end ) {
              if( content.charAt( start ) == ARG_DBL_QUOTE ) {
                // Two quotes together represents one quote
                buffer.append( ch );
                start++;
              } else {
                inQuotes = !inQuotes;
              }
            } else {
              // A lone quote at the end, just drop it.
              inQuotes = false;
            }
            break;
            
          default :
            buffer.append( ch );
            break;
        }
      }
      
      if( buffer.length() > 0 ) {
        alResult.add( buffer.toString() );
      }
        
      result = new String[ alResult.size() ];
      alResult.toArray( result );
    }
    return result;    
  } 
}