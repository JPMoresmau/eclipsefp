package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.util.ArrayList;
import java.util.List;

/**
 * Parses the output returned by Alex, Happy or UUAGC.
 *
 * @author Alejandro Serrano
 */
public class OutputParser {

	public static List<ProcessorError> errors(final String s) {
		ArrayList<ProcessorError> r = new ArrayList<ProcessorError>();

		String[] lines = s.split("[\r\n]+"); //$NON-NLS-1$
		for (String line : lines) {
		  String[] fileParts = line.split(":"); //$NON-NLS-1$

		  int current;
		  String fname = fileParts[0];
		  int lno, cno;
		  try {
		    // Try to find a line number
		    lno = Integer.parseInt( fileParts[1].trim() );
		    current = 1;
		  } catch (NumberFormatException e) {
		    // If not, it is a Windows path
		    fname = fname + ":" + fileParts[1]; //$NON-NLS-1$
		    current = 2;
		  }

		  // Parse line number
		  lno = Integer.parseInt( fileParts[current].trim() );
		  current++;

		  // Try to parse a column
		  try {
		    cno = Integer.parseInt( fileParts[current].trim() );
		    current++;
		  } catch (NumberFormatException e) {
		    cno = 0;
		  }

		  // The rest is the message
		  String msg = ""; //$NON-NLS-1$
		  for (int j = current; j < fileParts.length; j++) {
		    if (msg.length() > 0) {
		      msg += ":"; //$NON-NLS-1$
		    }
		    msg += fileParts[j];
		  }

			r.add(new ProcessorError(fname, lno, cno, msg));
		}

		return r;
	}
}
