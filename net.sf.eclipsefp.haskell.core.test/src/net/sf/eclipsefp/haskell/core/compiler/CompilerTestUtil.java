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
import java.io.StringWriter;
import junit.framework.Assert;

public class CompilerTestUtil extends Assert {

	public static ICompilerListener createListener() {
		ICompilerListener listener = createMock(ICompilerListener.class);
		final StringWriter out = new StringWriter();
		expect(listener.getOutputWriter()).andReturn(out).anyTimes();
		listener.startingCompilation();
		expectLastCall().anyTimes();
		replay(listener);
		return listener;
	}

	public static void assertReceivedExpectedOutput(final ICompilerListener listener) {
		assertEquals(StubCompiler.EXPECTED_STANDARD_OUTPUT + StubCompiler.EXPECTED_STANDARD_ERROR,
				     listener.getOutputWriter().toString());
	}

}
