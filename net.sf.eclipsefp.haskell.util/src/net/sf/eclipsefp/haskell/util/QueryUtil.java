// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.util;

import java.io.IOException;
import java.io.StringWriter;

/** Run a command with a single command line parameter and collect the resulting
 * output. A typical use would be to call a command line tool and ask for its
 * version number, e.g., 'java -version'.
 *
 * @author Leif Frenzel
 */
public class QueryUtil {

  /** Run a command with a single command line parameter, collect its output
   * and return the result as a string. If an exception occurs, then the
   * exception is converted to a string and returned instead.
   *
   * @param command The command to execute
   * @param param The single command line parameter
   * @return The command's output or an exception string, if an exception occurred.
   */
  public static String query( final String command, final String param ) {
    if( command == null || command.trim().length() == 0 ) {
      throw new IllegalArgumentException();
    }

    String result = new String();
    StringWriter output = new StringWriter();
    StringWriter errors = new StringWriter();

    ProcessRunner runner = new ProcessRunner();
    try {
      runner.executeBlocking( null, output, errors, command, param );
      result = output.toString();
    } catch( Exception ex ) {
      result = ex.toString();
    }
    return result;
  }

  /** Execute a command with a single command line parameter, returning the command's output as a string,
   * if the error stream is empty. If the error stream is not empty, return it instead.
   *
   * @param command The command to execute
   * @param param The single command line parameter.
   * @return The command's output as a string, or the error stream as a string, if the error stream is
   * not empty.
   * @throws IOException
   */
  public static String queryEx( final String command,
                                final String param ) throws IOException {
    if( command == null || command.trim().length() == 0 ) {
      throw new IllegalArgumentException();
    }

    String result = ""; //$NON-NLS-1$

    StringWriter output = new StringWriter();
    StringWriter errors = new StringWriter();
    ProcessRunner runner = new ProcessRunner();

    runner.executeBlocking( null, output, errors, command, param );

    result = output.toString();
    String errMsg = errors.toString();
    if( errMsg != null && errMsg.trim().length() > 0 ) {
      throw new IOException( errMsg );
    }

    return result;
  }
}