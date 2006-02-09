package net.sf.eclipsefp.haskell.core.jparser.test.doubles;

import java.io.PrintStream;

import junit.framework.Assert;

public class MockPrintStream extends PrintStream {
	
	public MockPrintStream(PrintStream stream) {
		super(stream);
	}

	@Override
	public void write(byte[] buf, int off, int len) {
		super.write(buf, off, len);
		Assert.fail("PrintStream used [" + new String(buf, off, len) + ']');
	}
	
}
