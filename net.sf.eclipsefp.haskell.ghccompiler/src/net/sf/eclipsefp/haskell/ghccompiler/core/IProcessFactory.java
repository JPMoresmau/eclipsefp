package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.IOException;

public interface IProcessFactory {

	Process startProcess(File workingDir, String[] commandLine) throws IOException;

}
