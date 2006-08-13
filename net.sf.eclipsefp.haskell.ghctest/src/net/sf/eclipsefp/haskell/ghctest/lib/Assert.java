package net.sf.eclipsefp.haskell.ghctest.lib;

import static org.junit.Assert.*;

import java.util.regex.Pattern;

public class Assert {

	public static void assertCommandMatches(String expectedRegex, String command) {
		Pattern expectedPattern = Pattern.compile(expectedRegex);
		String commandOutput = run(command);
		assertTrue(expectedPattern.matcher(commandOutput).find());
	}
	
	public static void assertCommandOutput(String expectedOutput, String command) {
		assertEquals(expectedOutput, run(command));
	}

	private static String run(String command) {
		return new CommandRunner().run(command);
	}
	
}
