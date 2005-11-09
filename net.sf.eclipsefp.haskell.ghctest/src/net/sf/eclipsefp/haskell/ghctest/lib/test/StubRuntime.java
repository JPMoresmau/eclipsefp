package net.sf.eclipsefp.haskell.ghctest.lib.test;

import java.io.IOException;

import net.sf.eclipsefp.haskell.ghctest.lib.RunnerRuntime;

public class StubRuntime extends RunnerRuntime {
	
	private Process fProcess;
	private IOException fException = null;

	public StubRuntime(String processOutput) {
		fProcess = new StubProcess(processOutput);
	}

	public StubRuntime(IOException exception) {
		fProcess = new StubProcess(exception);
	}

	public Process exec(String command) throws IOException {
		if (null != fException)
			throw fException;
		return fProcess;
	}

	public void throwException(IOException exception) {
		fException = exception;
	}

}
