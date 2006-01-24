package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringBufferInputStream;

import junit.framework.Assert;

public class MockInputStream extends InputStream {

	private InputStream fInput;
	private int fTimesClosed = 0;
	
	public MockInputStream(StringBufferInputStream stream) {
		fInput = stream;
	}

	@Override
	public int read() throws IOException {
		return fInput.read();
	}

	@Override
	public void close() throws IOException {
		fTimesClosed ++;
		fInput.close();
	}

	public void verify() {
		if (fTimesClosed != 1)
			Assert.fail("Stream not closed.");
	}

}
