// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.core;

import net.sf.eclipsefp.haskell.util.QueryUtil;

/** <p>contains some helping functionality.</p>
  *  
  * @author Leif Frenzel
  */
public class HaddockUtil {

  public static String queryHaddockExecutable( final String info ) {
    StringBuffer sb = new StringBuffer();
    String executable = ( info == null ) ? "" : info; 
    sb.append( QueryUtil.query( executable, IHaddockParameters.VERSION ) );
    return sb.toString();
  }
}
