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
		final String command = "ghc --version";
		
		CommandRunner runner = new CommandRunner(stubRuntime);
		String actualOutput = runner.run(command);
		
		assertEquals(expectedOutput, actualOutput);
	}
	
	@Test public void shouldReturnEmptyOutputOnProcessCreationError() {
		final StubRuntime stubRuntime = new StubRuntime("uninmportant");
		stubRuntime.throwException(new IOException("No privilege"));

		assertEquals("", new CommandRunner(stubRuntime).run("ghc --version"));
	}
	
	@Test public void shoudlReturnEmptyOutputOnProcessReadingError() {
		final StubRuntime stubRuntime = new StubRuntime(new IOException(
				"Read error"));
		
		assertEquals("", new CommandRunner(stubRuntime).run("ghc --version"));
	}
	
}
