package net.sf.eclipsefp.haskell.ghctest.lib.test;

import static org.junit.Assert.*;

import java.io.IOException;

import net.sf.eclipsefp.haskell.ghctest.lib.CommandRunner;
import net.sf.eclipsefp.haskell.ghctest.lib.RunnerRuntime;

import org.junit.Test;

public class CommandRunnerTest {
	
	@Test public void shouldCaptureOutput() throws IOException, InterruptedException {
		final String expectedOutput =
			"The Glorious Glasgow Haskell Compilation System, version 6.4.2\r\n";
		final RunnerRuntime stubRuntime = new StubRuntime(expectedOutput);
		final String command = "ghc --version";
		
		CommandRunner runner = new CommandRunner(stubRuntime);
		String actualOutput = runner.run(command);
		
		assertEquals(expectedOutput, actualOutput);
	}
	
	//TODO decide what to do with exception thrown by Runtime.exec

}
