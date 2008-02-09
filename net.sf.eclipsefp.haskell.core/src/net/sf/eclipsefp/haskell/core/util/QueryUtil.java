// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.util;

import java.io.StringWriter;
import net.sf.eclipsefp.haskell.core.internal.util.StreamMultiplexer;


/** <p>provides helping functionality to query an external tool for
  * information that it writes to the standard output.</p>
  * 
  * <p>A typical use would be to call a command line tool and ask for its
  * version number, like 'java -version'.</p>
  * 
  * @author Leif Frenzel
  */
public class QueryUtil {

  public static String query( final String command, final String param ) {
    if( command == null || command.trim().length() == 0 ) {
      throw new IllegalArgumentException();
    }
    
    String result = "";
    String[] cmdLine = new String[] { command, param };
    try {
      StringWriter output = new StringWriter();
      Process proc = Runtime.getRuntime().exec( cmdLine );
      Thread outRedirect = new StreamMultiplexer("output_redirect", 
                                                 proc.getInputStream(), 
                                                 output );
      outRedirect.start();
      proc.waitFor(); // wait for command to finish
      outRedirect.join();      // wait until out stream content is redirected
      result = output.toString();
    } catch( Exception ex ) {
      result = ex.toString();
    } 
    return result;
  }
}