package net.sf.eclipsefp.haskell.ghctest.lib.test;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringBufferInputStream;

public class StubProcess extends Process {

	private static class FailureInputStream extends InputStream {

		private IOException fException;

		public FailureInputStream(IOException exception) {
			fException = exception;
		}

		@Override
		public int read() throws IOException {
			throw fException;
		}

	}

	private InputStream fStdInputStream;
	private InputStream fErrInputStream;

	public StubProcess(String processStdOutput, String processErrOutput) {
		fStdInputStream = new StringBufferInputStream(processStdOutput);
		fErrInputStream = new StringBufferInputStream(processErrOutput);
	}

	public StubProcess(IOException exception) {
		fStdInputStream = fErrInputStream = new FailureInputStream(exception);
	}

	@Override
	public void destroy() {
	}

	@Override
	public int exitValue() {
		return 0;
	}

	@Override
	public InputStream getErrorStream() {
		return fErrInputStream;
	}

	@Override
	public InputStream getInputStream() {
		return fStdInputStream;
	}

	@Override
	public OutputStream getOutputStream() {
		return null;
	}

	@Override
	public int waitFor() throws InterruptedException {
		return 0;
	}

}
