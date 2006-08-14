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

import org.junit.Test;

import net.sf.eclipsefp.haskell.ghctest.lib.CompilationTestCase;

public class ErrorOutputTest extends CompilationTestCase {
	
	@Test public void shouldPointOffendingRangeWithErrorSpansFlag() throws IOException {
		File file = createSourceFile("Main.hs",
				                     "module Main where\n" +
				                     "\n" +
				                     "main = putStrLn $ show $ fac 5");
		final String expectedOutput = "Main.hs:3:25-27:";
		assertCompilationMatches(expectedOutput, file, "-ferror-spans");
	}
	
	@Test public void shouldSkipOneLineBetweenErrors() throws IOException {
		File file = createSourceFile("Main.hs",
                                     "module Main where\n" +
                                     "\n" +
                                     "main = putStrLn $ show $ fib (fac 5)");
		final String expectedOutput = "\r?\n" +
				                      "[^`]*`fib'\r?\n" +
				                      "\r?\n" +
				                      "[^`]*`fac'\r?\n";
		assertCompilationMatches(expectedOutput, file, "");
	}

}
