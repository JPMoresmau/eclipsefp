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
package net.sf.eclipsefp.haskell.core.compiler;

import junit.framework.TestCase;
import org.eclipse.core.resources.IFile;

public class CompilerManager_PDETest extends TestCase {

	private ICompilerManager manager;

	@Override
	protected void setUp() throws Exception {
		manager = CompilerManager.getInstance();
		String compilerID = "stub";
		manager.installCompiler(compilerID, new StubCompiler());
		manager.selectCompiler(compilerID);
	}

	public void testNotifiesCompilerListeners() {
		ICompilerListener listener = CompilerTestUtil.createListener();
		manager.addCompilerListener(listener);

		manager.getCompiler().compile((IFile) null);

		CompilerTestUtil.assertReceivedExpectedOutput(listener);
	}

	public void testDoNotNotifyRemovedListeners() {
		ICompilerListener listener = CompilerTestUtil.createListener();
		manager.addCompilerListener(listener);
		manager.removeCompilerListener(listener);

		manager.getCompiler().compile((IFile) null);

		assertEquals(0, listener.createOutputWriter().toString().length());
	}

}
