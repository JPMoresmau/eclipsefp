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

import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject_PDETestCase;
import org.eclipse.core.runtime.CoreException;

public class GhcCompilerTest_PDETest extends HaskellProject_PDETestCase {

	public void testParseOneErrorResult() throws CoreException {
	  // TODO currently broken due to GhcCompiler refactorings
	  /*
		IProcessRunner procRunner = createMock(IProcessRunner.class);
		expect(procRunner.execute((File) anyObject(), (Writer) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject()))
			.andReturn("\nMain.hs:1:25-27: Not in scope: `fat'\n");
		replay(procRunner);

		IFile f = createSourceFile("main = putStrLn $ show $ fat 4", "Main.hs");
		IHaskellCompiler compiler = new GhcCompiler(procRunner);
		compiler.compile(f);
		verify(procRunner);
		*/
	}
}
