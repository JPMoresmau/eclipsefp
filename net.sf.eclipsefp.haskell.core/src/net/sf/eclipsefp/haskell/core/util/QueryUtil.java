// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.util;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.StringWriter;
import net.sf.eclipsefp.haskell.core.internal.util.StreamRedirect;


/** <p>provides helping functionality to query an external tool for
  * information that it writes to the standard output.</p>
  *
  * <p>A typical use would be to call a command line tool and ask for its
  * version number, like 'java -version'.</p>
  *
  * TODO TtC refactor this to use ProcessRunner (and probably refactor ProcessRunner as well)
  *
  * @author Leif Frenzel
  */
public class QueryUtil {

  public static String query( final String command, final String param ) {
    if( command == null || command.trim().length() == 0 ) {
      throw new IllegalArgumentException();
    }

    String result = ""; //$NON-NLS-1$
    String[] cmdLine = new String[] { command, param };
    try {
      StringWriter output = new StringWriter();
      Process proc = Runtime.getRuntime().exec( cmdLine );
      Thread outRedirect = new StreamRedirect("output_redirect", //$NON-NLS-1$
                                                 new InputStreamReader(proc.getInputStream()),
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

  public static String queryEx( final String command,
                                final String param ) throws IOException {
    if( command == null || command.trim().length() == 0 ) {
      throw new IllegalArgumentException();
    }

    String result = ""; //$NON-NLS-1$
    String[] cmdLine = new String[] { command, param };

    StringWriter output = new StringWriter();
    StringWriter errors = new StringWriter();
    Process proc = Runtime.getRuntime().exec( cmdLine );
    Thread outRedirect = new StreamRedirect( "output_redirect", //$NON-NLS-1$
        new InputStreamReader(proc.getInputStream()),
                                                output );
    Thread errRedirect = new StreamRedirect( "error_redirect", //$NON-NLS-1$
        new InputStreamReader(proc.getErrorStream()),
                                                errors );
    outRedirect.start();
    errRedirect.start();
    try {
      proc.waitFor(); // wait for command to finish
      outRedirect.join();      // wait until out stream content is redirected
      errRedirect.join();      // wait until err stream content is redirected
    } catch( final InterruptedException irex ) {
      // ignore
    }
    result = output.toString();
    String errMsg = errors.toString();
    if( errMsg != null && errMsg.trim().length() > 0 ) {
      throw new IOException( errMsg );
    }
    return result;
  }
}