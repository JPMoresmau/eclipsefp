package net.sf.eclipsefp.haskell.util;

import java.io.File;
import java.io.IOException;

@Deprecated
public class ProcessFactory implements IProcessFactory {

	@Override
	public Process startProcess(final File workingDir, final String[] commandLine) throws IOException {
	  ProcessBuilder builder = new ProcessBuilder(commandLine);
	  builder.directory( workingDir );
	  builder.redirectErrorStream( true );
	  return builder.start();
	}

}
