package net.sf.eclipsefp.haskell.partitioned;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IPath;

public abstract class PartitionedRunner {
	
	public abstract String getExecutableName();
	
	public abstract InputStream selectStream(Process p);
	
	public List<ProcessorError> run(IPath path) {
		try {
			// Run the command
			String[] cmdLine = new String[] { getExecutableName(), path.toOSString() };
			Process p = Runtime.getRuntime().exec(cmdLine);
			// Parse the output
			p.waitFor();
			OutputParser parser = new OutputParser(selectStream(p));
			return parser.errors();
		} catch (Throwable ex) {
			return new ArrayList<ProcessorError>();
		}
	}
}
