package net.sf.eclipsefp.haskell.ghctest.lib;

import static org.junit.Assert.assertTrue;

import java.util.regex.Pattern;

public class Assert {

	public static void assertCommandMatches(String expectedRegex, String command) {
		String output = new CommandRunner().run(command);
		assertTrue(Pattern.compile(expectedRegex).matcher(output).find());
	}

}
