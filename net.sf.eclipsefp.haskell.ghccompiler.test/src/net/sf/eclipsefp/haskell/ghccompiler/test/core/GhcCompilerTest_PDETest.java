/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import static org.easymock.EasyMock.*;
import static net.sf.eclipsefp.haskell.ghccompiler.test.util.AssertCompilerOutput.*;

import java.io.File;
import java.io.Writer;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.test.internal.project.HaskellProject_PDETestCase;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcCompiler;
import net.sf.eclipsefp.haskell.ghccompiler.core.IProcessRunner;

public class GhcCompilerTest_PDETest extends HaskellProject_PDETestCase {
	
	public void testParseOneErrorResult() throws CoreException {
		IProcessRunner procRunner = createMock(IProcessRunner.class);
		expect(procRunner.execute((File) anyObject(), (Writer) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject()))
			.andReturn("\nMain.hs:1:25-27: Not in scope: `fat'\n");
		replay(procRunner);
		
		IFile f = createSourceFile("main = putStrLn $ show $ fat 4", "Main.hs");
		IHaskellCompiler compiler = new GhcCompiler(procRunner);
		ICompilerOutput output = compiler.compile(f);
		Collection<ICompilerOutputItem> errors = output.getErrors();
		
		assertEquals(1, errors.size());
		assertContains(1, 25, 27, "Not in scope: `fat'", errors);
		
		verify(procRunner);
	}
	
	public void testParseMakeFlagOneErrorResult() throws CoreException {
		IProcessRunner procRunner = createMock(IProcessRunner.class);
		expect(procRunner.execute((File) anyObject(), (Writer) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject()))
			.andReturn("Chasing modules from: Main.hs\n" +
					   "Compiling Main             ( Main.hs, Main.o )\n" +
					   "\n" +
					   "Main.hs:6:25-27: Not in scope: `fac'\n");
		replay(procRunner);

		IFile f = createSourceFile("main = putStrLn $ show $ fac 4", "Main.hs");
		IHaskellCompiler compiler = new GhcCompiler(procRunner);
		ICompilerOutput output = compiler.compile(f);
		Collection<ICompilerOutputItem> errors = output.getErrors();
	
		assertEquals(1, errors.size());
		assertContains(6, 25, 27, "Not in scope: `fac'", errors);
	}
	
}
