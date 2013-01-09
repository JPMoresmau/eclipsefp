package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses the output returned by Alex, Happy or UUAGC.
 *
 * @author Alejandro Serrano
 */
public class OutputParser {
  // capture groups: 1:file 2:line 3:col 4:msg
  static Pattern msg = Pattern.compile("^(.*?):([0-9]+)(?::([0-9]+))?:\\s*(.*)$"); //$NON-NLS-1$

  public static List<ProcessorError> errors( final String s ) {
    ArrayList<ProcessorError> r = new ArrayList<ProcessorError>();

    String[] lines = s.split( "[\r\n]+" ); //$NON-NLS-1$
    for( String line: lines ) {
      if (line.isEmpty()) {
        continue;
      }

      Matcher matcher = msg.matcher( line );
      if( matcher.matches() ) {
        String fname = matcher.group( 1 );
        int lno = 1;
        int cno = 1;
        String msg = matcher.group( 4 );

        try {
          lno = Integer.parseInt(matcher.group(2));
        } catch (NumberFormatException e) {
          // empty
        }

        try {
          cno = Integer.parseInt(matcher.group(3));
        } catch (NumberFormatException e) {
          // empty
        }

        r.add( new ProcessorError( fname, lno, cno, msg ) );
      } else {
        r.add( new ProcessorError( "", 1, 1, line ) ); //$NON-NLS-1$
      }
    }

    return r;
  }
}
