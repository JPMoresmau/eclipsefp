package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.util.ArrayList;
import java.util.List;

public class OutputParser {

	public static List<ProcessorError> errors(final String s) {
		ArrayList<ProcessorError> r = new ArrayList<ProcessorError>();

		String[] lines = s.split("[\r\n]+"); //$NON-NLS-1$
		for (String line : lines) {
			int parts = line.indexOf(": "); //$NON-NLS-1$

			String file = line.substring(0, parts);
			String msg = line.substring(parts + 2).trim();

			String[] fileParts = file.split(":"); //$NON-NLS-1$
			String fname;
			int lno, cno;

			if (fileParts.length == 4) {
			  // We have a Windows path + line + column
			  fname = fileParts[0] + ":" + fileParts[1]; //$NON-NLS-1$
			  lno = Integer.parseInt(fileParts[2]);
			  cno = Integer.parseInt(fileParts[3]);
			} else if (fileParts.length == 3) {
			  // Try to get it as Unix file + line + column
			  try {
  			  fname = fileParts[0];
  			  lno = Integer.parseInt(fileParts[1]);
          cno = Integer.parseInt(fileParts[2]);
			  } catch (NumberFormatException e) {
			    fname = fileParts[0] + ":" + fileParts[1]; //$NON-NLS-1$
	        lno = Integer.parseInt(fileParts[2]);
	        cno = 0;
			  }
			} else {
			  fname = fileParts[0];
				lno = Integer.parseInt(fileParts[1]);
				cno = 0;
			}

			r.add(new ProcessorError(fname, lno, cno, msg));
		}

		return r;
	}
}
