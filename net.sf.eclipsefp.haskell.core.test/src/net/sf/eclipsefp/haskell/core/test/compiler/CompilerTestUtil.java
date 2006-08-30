package net.sf.eclipsefp.haskell.core.test.compiler;

import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;

import java.io.StringWriter;

import junit.framework.Assert;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;

public class CompilerTestUtil extends Assert {

	public static ICompilerListener createListener() {
		ICompilerListener listener = createMock(ICompilerListener.class);
		final StringWriter out = new StringWriter();
		final StringWriter err = new StringWriter();
		expect(listener.getOutputWriter()).andReturn(out).anyTimes();
		expect(listener.getErrorWriter()).andReturn(err).anyTimes();
		replay(listener);
		return listener;
	}

	public static void assertReceivedExpectedOutput(ICompilerListener listener) {
		assertEquals(StubCompiler.EXPECTED_STANDARD_OUTPUT, listener.getOutputWriter().toString());
		assertEquals(StubCompiler.EXPECTED_STANDARD_ERROR, listener.getErrorWriter().toString());
	}

}
