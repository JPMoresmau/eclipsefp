package net.sf.eclipsefp.haskell.ghctest.lib.test;

import java.io.IOException;

import net.sf.eclipsefp.haskell.ghctest.lib.RunnerRuntime;

public class StubRuntime extends RunnerRuntime {
	
	private Process fProcess;

	public StubRuntime(String processOutput) {
		fProcess = new StubProcess(processOutput);
	}

	public Process exec(String command) throws IOException {
		return fProcess;
	}

}
