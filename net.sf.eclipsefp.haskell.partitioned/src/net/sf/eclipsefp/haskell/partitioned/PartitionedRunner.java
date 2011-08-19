package net.sf.eclipsefp.haskell.partitioned;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

public abstract class PartitionedRunner {
	
	public abstract String getExecutableName();
	
	public List<Error> run(IPath path) {
		try {
			// Run the command
			String[] cmdLine = new String[] { getExecutableName(), path.toOSString() };
			Process p = Runtime.getRuntime().exec(cmdLine);
			// Parse the output
			p.waitFor();
			OutputParser parser = new OutputParser(p.getInputStream());
			return parser.errors();
		} catch (Throwable ex) {
			return new ArrayList<Error>();
		}
	}
}
