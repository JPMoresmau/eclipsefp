// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.code;

/** <p>helping class that generates Haskell source code.</p>
  *
  * @author Leif Frenzel
  */
class CodeGenerator {

  // TODO honor code template for generating this
  static String createModuleContent( final String[] folderNames,
                                     final String name ) {
    StringBuffer sb = new StringBuffer();
    sb.append( getLineDelimiter() );
    sb.append( "module " );
    for( int i = 0; i < folderNames.length; i++ ) {
      sb.append( folderNames[ i ] );
      sb.append( "." );
    }
    sb.append( name );
    sb.append( " where" );
    sb.append( getLineDelimiter() );
    return sb.toString();
  }

  
  // helping methods
  //////////////////
  
  private static String getLineDelimiter() {
    return System.getProperty( "line.separator", "\n" );    
  }
}
