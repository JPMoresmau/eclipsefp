package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.IOException;

public class ProcessFactory implements IProcessFactory {

	public Process startProcess(File workingDir, String[] commandLine) throws IOException {
		return Runtime.getRuntime().exec(commandLine, null, workingDir);
	}

}
