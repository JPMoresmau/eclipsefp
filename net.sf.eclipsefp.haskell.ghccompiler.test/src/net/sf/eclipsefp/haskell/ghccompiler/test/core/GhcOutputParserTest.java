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

import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
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

}
