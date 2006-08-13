package net.sf.eclipsefp.haskell.ghctest;

import static net.sf.eclipsefp.haskell.ghctest.lib.Assert.*;

import java.io.File;
import java.io.IOException;

import net.sf.eclipsefp.haskell.ghctest.lib.TestingDirectory;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class SuccessfulOutputTest {
	
	private TestingDirectory fTempDir;

	@Test public void flagVersionSaysVersionNumber() {
		assertCommandMatches("The Glorious Glasgow Haskell Compilation System, version",
				             "ghc --version");
	}
	
	@Test public void supportsErrorSpansFlag() throws IOException {
		assertCompilationOutput("", createHelloWorldFile(), "-ferror-spans");
	}

	@Test public void supportsMakeFlag() throws IOException {
		final String expectedOutput = "Chasing modules from: [^\r\n]*\r?\n" +
						              "Compiling Main\\s+\\( [^,]+, [^\\)]+\\)\r?\n" +
						              "Linking ...";
		assertCompilationMatches(expectedOutput, createHelloWorldFile(), "--make");
	}
	
	private void assertCompilationMatches(String expectedOutput, File file, String options) throws IOException {
		final String parentDirectory = fTempDir.getPathname().getCanonicalPath();
		assertCommandMatches(expectedOutput,
				            "ghc " + options + " "
				           + file.getCanonicalPath()
				           + " -odir " + parentDirectory
				           + " -o " + parentDirectory + "/a.out");
	}

	private void assertCompilationOutput(String expectedOutput,
			                             File file,
			                             String options)
	throws IOException
	{
		final String parentDirectory = fTempDir.getPathname().getCanonicalPath();
		assertCommandOutput(expectedOutput,
				            "ghc " + options + " "
				           + file.getCanonicalPath()
				           + " -odir " + parentDirectory
				           + " -o " + parentDirectory + "/a.out");
	}

	private File createHelloWorldFile() throws IOException {
		return createSourceFile("Main.hs", "module Main where\n" +
						                   "\n" +
						                   "main = putStrLn \"Hello, world!\"");
	}
	
	private File createSourceFile(String fileName, String contents) throws IOException {
		return fTempDir.createFile(fileName, contents);
	}
	
	@Before public void initializeTempTestDirectory() {
		fTempDir = new TestingDirectory();
	}
	
	@After public void clearTempTestDirectory() {
		fTempDir.destroy();
	}
	
}
