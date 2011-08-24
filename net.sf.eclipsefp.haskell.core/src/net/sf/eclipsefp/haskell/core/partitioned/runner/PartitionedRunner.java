package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.runtime.IPath;

public abstract class PartitionedRunner {

	public abstract String getExecutableName();

	public abstract StringWriter selectStream(StringWriter out, StringWriter err);

	public List<ProcessorError> run(final IPath path) {
		try {
			// Run the command
			StringWriter out=new StringWriter();
			StringWriter err=new StringWriter();
      new ProcessRunner().executeBlocking(path.toFile().getParentFile(), out, err,  getExecutableName(), path.toOSString());
      // Parse the output
      return OutputParser.errors(selectStream( out, err ).toString());
		} catch (Throwable ex) {
			return new ArrayList<ProcessorError>();
		}
	}
}
