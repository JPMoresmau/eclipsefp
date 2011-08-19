package net.sf.eclipsefp.haskell.partitioned;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

public class OutputParser {
	
	BufferedReader reader;	
	
	public OutputParser(InputStream stream) {
		reader = new BufferedReader(new InputStreamReader(stream));
	}
	
	public List<Error> errors() {
		ArrayList<Error> r = new ArrayList<Error>();
		
		try {
			String line;
			while ((line = reader.readLine()) != null) {
				String[] parts = line.split(":");
				String file = parts[0];
				int lno = Integer.parseInt(parts[1]);
				int cno = Integer.parseInt(parts[2]);
				String msg = parts[3].trim();
				r.add(new Error(file, lno, cno, msg));
			}
		} catch (IOException e) {
			// Do nothing
		}
		
		return r;
	}
}
