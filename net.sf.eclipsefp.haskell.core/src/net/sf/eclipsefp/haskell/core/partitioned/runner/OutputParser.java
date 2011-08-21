package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class OutputParser {
	
	InputStream stream;	
	
	public OutputParser(InputStream stream) {
		this.stream = stream;
	}
	
	public List<ProcessorError> errors() {
		ArrayList<ProcessorError> r = new ArrayList<ProcessorError>();
		
		String s = new Scanner(stream).useDelimiter("\\Z").next();
		String[] lines = s.split("[\r\n]+");
		for (String line : lines) {
			int parts = line.indexOf(": ");
			
			String file = line.substring(0, parts);
			String msg = line.substring(parts + 2).trim();
			
			String[] fileParts = file.split(":");
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
