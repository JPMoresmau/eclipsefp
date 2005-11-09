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

	private InputStream fInputStream;

	public StubProcess(String processOutput) {
		fInputStream = new StringBufferInputStream(processOutput);
	}

	public StubProcess(IOException exception) {
		fInputStream = new FailureInputStream(exception);
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
		return null;
	}

	@Override
	public InputStream getInputStream() {
		return fInputStream;
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
