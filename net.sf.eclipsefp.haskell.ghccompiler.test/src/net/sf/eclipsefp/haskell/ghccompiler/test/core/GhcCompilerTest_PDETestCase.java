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

import java.io.File;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.compiler.AbstractHaskellCompiler;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutputItem;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcCompiler;
import net.sf.eclipsefp.haskell.ghccompiler.core.IProcessRunner;
import net.sf.eclipsefp.test.util.haskell.HaskellProject_PDETestCase;

public class GhcCompilerTest_PDETestCase extends HaskellProject_PDETestCase {
	
	public void testParseResults() throws CoreException {
		IProcessRunner procRunner = createMock(IProcessRunner.class);
		expect(procRunner.execute((File) anyObject(),(String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject(), (String) anyObject()))
			.andReturn("\nMain.hs:1:25-27: Not in scope: `fat'\n");
		replay(procRunner);
		
		IFile f = createSourceFile("main = putStrLn $ show $ fat 4", "Main.hs");
		AbstractHaskellCompiler compiler = new GhcCompiler(procRunner);
		ICompilerOutput output = compiler.compile(f);
		Collection<ICompilerOutputItem> errors = output.getErrors();
		
		assertEquals(1, errors.size());
		assertContains(1, 25, 27, "Not in scope: `fat'", errors);
		
		verify(procRunner);
	}

	private void assertContains(int line, int startColumn, int endColumn,
	    String message, Collection<ICompilerOutputItem> errors)
	{
		for(ICompilerOutputItem item : errors) {
			if (   line == item.getLine()
			   && startColumn == item.getStartColumn()
			   && endColumn == item.getEndColumn()
			   && message.equals(item.getComment()) )
			{
				return;
			}
		}
		fail(String.format("Could not find error on line %d, range (%d, %d)" +
				           "and message %s", line, startColumn, endColumn,
				           message));
	}

}
