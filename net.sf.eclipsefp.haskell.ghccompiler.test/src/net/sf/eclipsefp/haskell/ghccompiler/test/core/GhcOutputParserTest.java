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

import static net.sf.eclipsefp.haskell.ghccompiler.test.util.AssertCompilerOutput.assertContains;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcOutputParser;

public class GhcOutputParserTest extends TestCase {

	public void testOneSingleLineError() throws IOException {
    List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "001" ) );
		assertContains(3, 25, 27, "Not in scope: `fac'", list );
	}

	public void testOneCharacterErrorSpan() throws IOException {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "002" ) );
		assertContains(3, 25, 25, "Not in scope: `f'", list );
	}

	public void testWoirdCompilingAppearsOnErrorMessage() throws IOException {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "003" ) );
		assertContains(3, 25, 33, "Not in scope: `Compiling'", list );
	}

	public void testMultipleErrors() throws IOException {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "004" ) );
		assertContains(4, 26, 28, "Not in scope: `fac'", list );
		assertContains(4, 32, 34, "Not in scope: `fib'", list );
	}

	public void testIgnoresSingleLineMakeFlagOutput() throws IOException {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "005" ) );
		Collection<ICompilerOutputItem> errors = list ;
		assertEquals(1, errors.size());
		assertContains(4, 25, 27, "Not in scope: `fac'", errors);
	}

	public void testIgnoresMultiLineMakeFlagOutput() throws IOException {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "006" ) );
		assertContains(6, 25, 27, "Not in scope: `fib'", list );
	}

	public void testIgnoresSkippedModulesOnMakeFlagOutput() throws IOException {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "007" ) );
		assertContains(6, 25, 27, "Not in scope: `fib'", list );
	}

	public void testParseInputWithWindowsStyleFilePaths() throws IOException {
	  List<ICompilerOutputItem> list = GhcOutputParser.parse( readFile( "008" ) );
		assertContains(4, 28, 30, "Not in scope: `fac'", list );
	}


	// helping functions
	////////////////////

	private String readFile( final String name ) throws IOException {
	  ClassLoader cl = getClass().getClassLoader();
    String resName = "res/" + name;
    InputStream is = cl.getResourceAsStream( resName );
	  return ResourceUtil.readStream( is );
	}
}
