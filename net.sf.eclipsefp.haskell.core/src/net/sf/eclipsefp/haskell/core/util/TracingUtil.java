// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.util;

import java.util.Iterator;
import java.util.List;

/** <p>contains helping functionality used in tracing and logging.</p>
  *
  * @author Leif Frenzel
  */
public class TracingUtil {

  public static void dump( final List cmdLine ) {
    StringBuffer sb = new StringBuffer();
    Iterator iter = cmdLine.iterator();
    while( iter.hasNext() ) {
      sb.append( iter.next() );
      sb.append( " " );
    }
    System.out.println( sb.toString() );
  }
}
