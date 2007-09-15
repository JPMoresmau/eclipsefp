/* *****************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 * *****************************************************************************/
package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

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
		fStdInputStream = new ByteArrayInputStream(processStdOutput.getBytes());
		fErrInputStream = new ByteArrayInputStream(processErrOutput.getBytes());
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
