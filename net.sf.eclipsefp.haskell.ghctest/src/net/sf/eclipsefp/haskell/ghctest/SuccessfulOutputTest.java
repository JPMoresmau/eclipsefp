package net.sf.eclipsefp.haskell.ghctest;

import static net.sf.eclipsefp.haskell.ghctest.lib.Assert.*;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

import org.junit.Test;

public class SuccessfulOutputTest {
	
	@Test public void flagVersionSaysVersionNumber() {
		assertCommandMatches("The Glorious Glasgow Haskell Compilation System, version",
				             "ghc --version");
	}
	
	@Test public void supportsErrorSpansFlag() throws IOException {
		File file = new File(System.getProperty("java.io.tmpdir") + "/Main.hs");
		Writer output = new FileWriter(file);
		output.write("module Main where\n" +
				     "\n" +
				     "main = putStrLn \"Hello, world!\"");
		output.close();
		assertCommandOutput("",
				            "ghc -ferror-spans "
				           + file.getCanonicalPath()
				           + " -odir "
				           + System.getProperty("java.io.tmpdir"));
	}

}
