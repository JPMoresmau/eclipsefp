package net.sf.eclipsefp.haskell.core.partitioned.runner;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.runtime.IPath;

/**
 * Base class for Alex, Happy and UUAGC runners, the classes
 * which build the arguments list, calls the corresponding
 * executable and parses the results back.
 *
 * @author Alejandro Serrano
 */
public abstract class PartitionedRunner {

  /**
   * The executable to call to run the tool.
   * @return
   */
	public abstract String getExecutableName();

	/**
	 * Allows to select if output comes from stdout or stderr.
	 * @param out Represents stdout stream.
	 * @param err Represents stderr stream.
	 * @return The stream to parse.
	 */
  public StringWriter selectStream( final StringWriter out, final StringWriter err ) {
    return err.toString().length() > 0 ? err : out;
  }

	public List<ProcessorError> run(final IPath path) {
		try {
			// Run the command
			StringWriter out=new StringWriter();
			StringWriter err=new StringWriter();
      new ProcessRunner().executeBlocking(path.toFile().getParentFile(), out, err,  getExecutableName(), path.toOSString());
      // Parse the output
      return OutputParser.errors(selectStream( out, err ).toString());
		} catch (Throwable ex) {
			return new ArrayList<>();
		}
	}
}
