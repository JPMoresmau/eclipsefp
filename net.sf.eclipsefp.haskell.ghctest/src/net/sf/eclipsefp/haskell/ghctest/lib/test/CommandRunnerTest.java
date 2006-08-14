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

import static org.junit.Assert.*;

import java.io.IOException;

import net.sf.eclipsefp.haskell.ghctest.lib.CommandRunner;
import net.sf.eclipsefp.haskell.ghctest.lib.RunnerRuntime;

import org.junit.Test;

public class CommandRunnerTest {
	
	@Test public void shouldCaptureOutput() {
		final String expectedOutput =
			"The Glorious Glasgow Haskell Compilation System, version 6.4.2\r\n";
		final RunnerRuntime stubRuntime = new StubRuntime(expectedOutput);
		
		CommandRunner runner = new CommandRunner(stubRuntime);
		String actualOutput = runner.run("uninportant");
		
		assertEquals(expectedOutput, actualOutput);
	}
	
	@Test public void shouldReturnEmptyOutputOnProcessCreationError() {
		final StubRuntime stubRuntime = new StubRuntime("uninmportant");
		stubRuntime.throwException(new IOException("No privilege"));

		assertEquals("", new CommandRunner(stubRuntime).run("unimportant"));
	}
	
	@Test public void shouldReturnEmptyOutputOnProcessReadingError() {
		final StubRuntime stubRuntime = new StubRuntime(
				                            new IOException("Read error"));
		
		assertEquals("", new CommandRunner(stubRuntime).run("unimportant"));
	}
	
	@Test public void outputsErrorOutputStream() {
		final String expectedError =
			"\r\n" +
			"c:/temp/Main.hs:1:44: Not in scope: `fac'\r\n" +
			"\r\n";
		
		final StubRuntime stubRuntime = new StubRuntime("", expectedError);
		
		assertEquals(expectedError, new CommandRunner(stubRuntime).run("unimportant"));
	}
	
}
