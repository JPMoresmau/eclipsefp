package net.sf.eclipsefp.haskell.partitioned;

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
			String[] parts = line.split(":");
			String file = parts[0];
			int lno = Integer.parseInt(parts[1]);
			int cno = Integer.parseInt(parts[2]);
			String msg = parts[3].trim();
			r.add(new ProcessorError(file, lno, cno, msg));
		}
		
		return r;
	}
}
