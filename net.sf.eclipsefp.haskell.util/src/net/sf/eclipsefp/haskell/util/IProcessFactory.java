package net.sf.eclipsefp.haskell.util;

import java.io.File;
import java.io.IOException;

@Deprecated
public interface IProcessFactory {

	Process startProcess(File workingDir, String[] commandLine) throws IOException;

}
