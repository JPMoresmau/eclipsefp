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
package net.sf.eclipsefp.haskell.ghctest.lib.test;

import java.io.IOException;

import net.sf.eclipsefp.haskell.ghctest.lib.RunnerRuntime;

public class StubRuntime extends RunnerRuntime {
	
	private Process fProcess;
	private IOException fException = null;

	public StubRuntime(String processStdOutput, String processErrOutput) {
		fProcess = new StubProcess(processStdOutput, processErrOutput);
	}

	public StubRuntime(String processStdOutput) {
		this(processStdOutput, "");
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
