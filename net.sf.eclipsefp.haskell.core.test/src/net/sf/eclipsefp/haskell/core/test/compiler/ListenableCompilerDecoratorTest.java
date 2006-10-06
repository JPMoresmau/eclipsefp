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
package net.sf.eclipsefp.haskell.core.test.compiler;

import static net.sf.eclipsefp.haskell.core.test.compiler.CompilerTestUtil.*;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;

import java.io.StringWriter;

import org.eclipse.core.resources.IFile;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.compiler.ListenableCompilerDecorator;
import junit.framework.TestCase;

public class ListenableCompilerDecoratorTest extends TestCase {
	
	public void testRedirectsCompilerOutputToOneListener() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = createListener();
		
		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);
		
		testedCompiler.compile((IFile) null);
		
		assertReceivedExpectedOutput(listener);
	}

	public void testRedirectsCompilerOutputToMoreThanOneListener() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener fstListener = createListener();
		ICompilerListener sndListener = createListener();
		ICompilerListener trdListener = createListener();
		
		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(fstListener);
		testedCompiler.addListener(sndListener);
		testedCompiler.addListener(trdListener);
		
		testedCompiler.compile((IFile) null);
		
		assertReceivedExpectedOutput(fstListener);
		assertReceivedExpectedOutput(sndListener);
		assertReceivedExpectedOutput(trdListener);
	}
	
	public void testCompileMoreThanOnce() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = createListener();
		
		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);
		
		testedCompiler.compile((IFile) null);
		assertReceivedExpectedOutput(listener);

		ICompilerListener otherListener = createListener();
		testedCompiler.addListener(otherListener);
	
		testedCompiler.compile((IFile) null);
		assertReceivedExpectedOutput(otherListener);
	}
	
	public void testFiresCompilationStartingEvent() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = createMockListener();
		
		listener.startingCompilation();
		expectLastCall().once();
		
		replay(listener);
		
		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);
		
		testedCompiler.compile((IFile) null);		

		verify(listener);
	}

	private ICompilerListener createMockListener() {
		ICompilerListener listener = createMock(ICompilerListener.class);
		
		final StringWriter out = new StringWriter();
		final StringWriter err = new StringWriter();
		expect(listener.getOutputWriter()).andReturn(out).anyTimes();
		expect(listener.getErrorWriter()).andReturn(err).anyTimes();
		return listener;
	}

}
