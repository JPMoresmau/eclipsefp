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

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import java.io.File;
import java.io.Writer;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject_PDETestCase;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcCompiler;
import net.sf.eclipsefp.haskell.ghccompiler.core.IProcessRunner;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

public class GhcCompilerTest_PDETest extends HaskellProject_PDETestCase {

	public void testParseOneErrorResult() throws CoreException {
		IProcessRunner procRunner = createMock(IProcessRunner.class);
		expect(procRunner.execute((File) anyObject(), (Writer) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject()))
			.andReturn("\nMain.hs:1:25-27: Not in scope: `fat'\n");
		replay(procRunner);

		IFile f = createSourceFile("main = putStrLn $ show $ fat 4", "Main.hs");
		IHaskellCompiler compiler = new GhcCompiler(procRunner);
		compiler.compile(f);
		verify(procRunner);
	}
}
