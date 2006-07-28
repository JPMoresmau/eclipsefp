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
package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import static net.sf.eclipsefp.haskell.ghccompiler.test.util.AssertCompilerOutput.*;

import java.util.Collection;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcOutputParser;
import junit.framework.TestCase;

public class GhcOutputParserTest extends TestCase {
	
	public void testOneSingleLineError() {
		ICompilerOutput output = GhcOutputParser.parse(
		    "\nMain.hs:3:25-27: Not in scope: `fac'\n");
		
		assertContains(3, 25, 27, "Not in scope: `fac'", output.getErrors());
	}
	
	public void testMultipleErrors() {
		ICompilerOutput output = GhcOutputParser.parse(
		    "\nMain.hs:4:26-28: Not in scope: `fac'\n" +
		    "\n" +
		    "Main.hs:4:32-34: Not in scope: `fib'\n");

		assertContains(4, 26, 28, "Not in scope: `fac'", output.getErrors());
		assertContains(4, 32, 34, "Not in scope: `fib'", output.getErrors());
	}
	
	public void testIgnoresSingleLineMakeFlagOutput() {
		ICompilerOutput output = GhcOutputParser.parse(
			"Chasing modules from: Main.hs\n" +
			"Compiling Main             ( Main.hs, Main.o )\n" +
			"\n" +
			"Main.hs:4:25-27: Not in scope: `fac'\n");
		
		final Collection<ICompilerOutputItem> errors = output.getErrors();
		assertEquals(1, errors.size());
		assertContains(4, 25, 27, "Not in scope: `fac'", errors);
	}
	
	public void testIgnoresMultiLineMakeFlagOutput() {
		ICompilerOutput output = GhcOutputParser.parse(
		    "Chasing modules from: Main.hs\n" +
		    "Compiling Factorial        ( ./Factorial.hs, ./Factorial.o )\n" +
		    "Compiling Main             ( Main.hs, Main.o )\n" +
		    "\n" +
		    "Main.hs:6:25-27: Not in scope: `fib'\n");
		
		assertContains(6, 25, 27, "Not in scope: `fib'", output.getErrors());
	}
	
	public void testIgnoresSkippedModulesOnMakeFlagOutput() {
		ICompilerOutput output = GhcOutputParser.parse(
			    "Chasing modules from: Main.hs\n" +
			    "Skipping  Factorial        ( ./Factorial.hs, ./Factorial.o )\n" +
			    "Compiling Main             ( Main.hs, Main.o )\n" +
			    "\n" +
			    "Main.hs:6:25-27: Not in scope: `fib'\n");
			
			assertContains(6, 25, 27, "Not in scope: `fib'", output.getErrors());
	}
	
	public void testParseInputWithWindowsStyleLineBreaks() {
		ICompilerOutput output = GhcOutputParser.parse(
			    "Chasing modules from: Main.hs\r\n" +
			    "Compiling Main             ( Main.hs, C:\\programas\\eclipse\\platform\\3.2\\eclipse\\tmp\\runtime-workspace\\qsort\\out/Main.o )\r\n" +
			    "\r\n" +
			    "Main.hs:4:28-30: Not in scope: `fac'\r\n\n");	
		
		assertContains(4, 28, 30, "Not in scope: `fib'", output.getErrors());
	}
	
	//TODO write tests for the ghc output
	//TODO parse multi-line errors like this:
//
//	Main.hs:4:0-15:
//	    Failed to load interface for `Factorial':
//	        Could not find module `Factorial':
//	          use -v to see a list of the files searched for
	
	//TODO the make flag output may not be followed by the Compiling or Skipping
	//lines, like this: (here Factorial.hs didn't exist)
//	Chasing modules from: Main.hs
//	Could not find module `Factorial':
//	  use -v to see a list of the files searched for
//	  (imported from Main.hs)
	
	
}
