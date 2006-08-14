package net.sf.eclipsefp.haskell.ghctest.lib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.regex.Pattern;

import org.junit.After;
import org.junit.Before;

public class CompilationTestCase {
	
	public static void assertCommandMatches(String expectedRegex, String command) {
		Pattern expectedPattern = Pattern.compile(expectedRegex);
		String commandOutput = run(command);
		assertTrue(expectedPattern.matcher(commandOutput).find());
	}
	
	public static void assertCommandOutput(String expectedOutput, String command) {
		assertEquals(expectedOutput, run(command));
	}

	protected static String run(String command) {
		return new CommandRunner().run(command);
	}

	private TestingDirectory fTempDir;

	protected String compile(File file, String options) throws IOException {
		final String parentDirectory = fTempDir.getPathname().getCanonicalPath();
		String command = "ghc " + options + " "
		                 + file.getCanonicalPath()
		                 + " -i" + parentDirectory
		                 + " -odir " + parentDirectory
		                 + " -o " + parentDirectory + "/a.out";
		return run(command);
	}

	protected void assertCompilationMatches(String expectedRegex, File file, String options) throws IOException {
		Pattern expectedPattern = Pattern.compile(expectedRegex);
		String commandOutput = compile(file, options);
		assertTrue(expectedPattern.matcher(commandOutput).find());
	}

	protected void assertCompilationOutput(String expectedOutput, File file, String options) throws IOException {
		assertEquals(expectedOutput, compile(file, options));
	}

	protected File createSourceFile(String fileName, String contents) throws IOException {
		return fTempDir.createFile(fileName, contents);
	}

	@Before
	public void initializeTempTestDirectory() {
		fTempDir = new TestingDirectory();
	}

	@After
	public void clearTempTestDirectory() {
		fTempDir.destroy();
	}

}
