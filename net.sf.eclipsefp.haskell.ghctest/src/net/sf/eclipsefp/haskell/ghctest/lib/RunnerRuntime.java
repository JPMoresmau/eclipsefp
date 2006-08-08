package net.sf.eclipsefp.haskell.ghctest.lib;

import java.io.IOException;

public class RunnerRuntime {
	
	public Process exec(String command) throws IOException {
		return Runtime.getRuntime().exec(command);
	}

}
