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
			String fname = fileParts[0];
			int lno, cno;
			if (fileParts.length == 3) {
				// We have column name
				lno = Integer.parseInt(fileParts[1]);
				cno = Integer.parseInt(fileParts[2]);
			} else {
				lno = Integer.parseInt(fileParts[1]);
				cno = 0;
			}

			r.add(new ProcessorError(fname, lno, cno, msg));
		}

		return r;
	}
}
