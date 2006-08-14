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
package net.sf.eclipsefp.haskell.ghctest;

import java.io.File;
import java.io.IOException;

import net.sf.eclipsefp.haskell.ghctest.lib.CompilationTestCase;

import org.junit.Test;

public class SuccessfulOutputTest extends CompilationTestCase {
	
	private static final String MAIN_FACTORIAL_SRC =
		"module Main where\n" +
		"\n" +
		"import Factorial\n" +
		"\n" +
		"main = putStrLn $ show $ fac 4\n";
	private static final String FACTORIAL_SRC =
		"module Factorial where\n" +
		"\n" +
		"fac = foldr (*) 1 . enumFromTo 1";

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
	
	@Test public void multiModuleMakeFlagOutput() throws IOException {
		createSourceFile("Factorial.hs", FACTORIAL_SRC);
		File mainFile = createSourceFile("Main.hs", MAIN_FACTORIAL_SRC);
		final String expectedOutput = "Chasing modules from: [^\r\n]*\r?\n" +
                                      "Compiling Factorial\\s+\\( [^,]+, [^\\)]+\\)\r?\n" +
                                      "Compiling Main\\s+\\( [^,]+, [^\\)]+\\)\r?\n" +
                                      "Linking ...";
		assertCompilationMatches(expectedOutput, mainFile, "--make");
	}
	
	@Test public void shouldSkipModulesThatDontNeedRecompiling() throws IOException, InterruptedException {
		createSourceFile("Factorial.hs", FACTORIAL_SRC);
		File mainFile = createSourceFile("Main.hs", MAIN_FACTORIAL_SRC);
		compile(mainFile, "--make");
		mainFile = createSourceFile("Main.hs",
                                    "module Main where\n" +
                                    "\n" +
                                    "import Factorial\n" +
                                    "\n" +
                					"main = putStrLn $ show $ fac 5\n");
		final String expectedOutput = "Chasing modules from: [^\r\n]*\r?\n" +
                                      "Skipping\\s+Factorial\\s+\\( [^,]+, [^\\)]+\\)\r?\n";
		assertCompilationMatches(expectedOutput, mainFile, "--make");
	}
	
	private File createHelloWorldFile() throws IOException {
		return createSourceFile("Main.hs", "module Main where\n" +
						                   "\n" +
						                   "main = putStrLn \"Hello, world!\"");
	}
	
}
