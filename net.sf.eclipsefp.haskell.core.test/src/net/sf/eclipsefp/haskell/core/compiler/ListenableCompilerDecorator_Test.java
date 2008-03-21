/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Leif Frenzel - Initial API and implementation
 *     Thiago Arrais - Reestructuring and documentation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.core.compiler;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import java.io.StringWriter;
import junit.framework.TestCase;
import org.eclipse.core.resources.IFile;

public class ListenableCompilerDecorator_Test extends TestCase {

	public void testRedirectsCompilerOutputToOneListener() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = CompilerTestUtil.createListener();

		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);

		testedCompiler.compile((IFile) null);

		CompilerTestUtil.assertReceivedExpectedOutput(listener);
	}

	public void testRedirectsCompilerOutputToMoreThanOneListener() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener fstListener = CompilerTestUtil.createListener();
		ICompilerListener sndListener = CompilerTestUtil.createListener();
		ICompilerListener trdListener = CompilerTestUtil.createListener();

		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(fstListener);
		testedCompiler.addListener(sndListener);
		testedCompiler.addListener(trdListener);

		testedCompiler.compile((IFile) null);

		CompilerTestUtil.assertReceivedExpectedOutput(fstListener);
		CompilerTestUtil.assertReceivedExpectedOutput(sndListener);
		CompilerTestUtil.assertReceivedExpectedOutput(trdListener);
	}

	public void testCompileMoreThanOnce() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = CompilerTestUtil.createListener();

		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);

		testedCompiler.compile((IFile) null);
		CompilerTestUtil.assertReceivedExpectedOutput(listener);

		ICompilerListener otherListener = CompilerTestUtil.createListener();
		testedCompiler.addListener(otherListener);

		testedCompiler.compile((IFile) null);
		CompilerTestUtil.assertReceivedExpectedOutput(otherListener);
	}

	public void testFiresCompilationStartingEvent() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = createMockListener();

		listener.startingCompilation();
		expectLastCall().once();

		replay(listener);

		ListenableCompilerDecorator underTestCompiler =
			new ListenableCompilerDecorator(realCompiler);
		underTestCompiler.addListener(listener);

		underTestCompiler.compile((IFile) null);

		verify(listener);
	}

	public void testDoNotReportCompilationStartToRemovedListeners() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = createMockListener();

		listener.startingCompilation();
		expectLastCall().once();
		replay(listener);

		ListenableCompilerDecorator underTestCompiler =
			new ListenableCompilerDecorator(realCompiler);

		underTestCompiler.addListener(listener);
		underTestCompiler.compile((IFile) null);

		underTestCompiler.removeListener(listener);
		underTestCompiler.compile((IFile) null);

		verify(listener);
	}

	public void testDoNotWriteToRemovedListenersStreams() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = CompilerTestUtil.createListener();

		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);
		testedCompiler.removeListener(listener);

		testedCompiler.compile((IFile) null);
		assertEquals(0, listener.getOutputWriter().toString().length());
	}

	private ICompilerListener createMockListener() {
		ICompilerListener listener = createMock(ICompilerListener.class);

		final StringWriter out = new StringWriter();
		expect(listener.getOutputWriter()).andReturn(out).anyTimes();
		return listener;
	}

}
