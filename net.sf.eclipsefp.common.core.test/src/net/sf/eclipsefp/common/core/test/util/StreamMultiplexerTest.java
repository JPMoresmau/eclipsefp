package net.sf.eclipsefp.common.core.test.util;

import java.io.IOException;
import java.io.StringBufferInputStream;
import java.io.StringWriter;
import java.io.Writer;

import net.sf.eclipsefp.common.core.util.StreamMultiplexer;
import junit.framework.TestCase;

public class StreamMultiplexerTest extends TestCase {
	
	private static class ProblematicWriter extends Writer {

		@Override
		public void close() throws IOException { }

		@Override
		public void flush() throws IOException { }

		@Override
		public void write(char[] cbuf, int off, int len) throws IOException {
			throw new IOException();
		}

	}

	private final String EXPECTED_CONTENTS = "expected contents";
	private StringBufferInputStream fInput =
		new StringBufferInputStream(EXPECTED_CONTENTS);

	public void testMultiplexesToOneOutput() throws InterruptedException, IOException {
		final StringWriter out = new StringWriter();
		multiplexTo(out);
		assertEquals(EXPECTED_CONTENTS, out.toString());
	}
	
	public void testMultiplexesWithMoreThanOneOutput() throws InterruptedException, IOException {
		final StringWriter fstOut = new StringWriter();
		final StringWriter sndOut = new StringWriter();
		multiplexTo(fstOut, sndOut);
		assertEquals(EXPECTED_CONTENTS, fstOut.toString());
		assertEquals(EXPECTED_CONTENTS, sndOut.toString());
	}
	
	public void testWritingErrorIsolatedFromOtherWriters() throws InterruptedException, IOException {
		final StringWriter fstOut = new StringWriter();
		final Writer sndOut = new ProblematicWriter();
		final StringWriter trdOut = new StringWriter();
		multiplexTo(fstOut, sndOut, trdOut);
		assertEquals(EXPECTED_CONTENTS, fstOut.toString());
		assertEquals(EXPECTED_CONTENTS, trdOut.toString());
	}

	private void multiplexTo(Writer... outputs) throws InterruptedException, IOException {
		Thread multiplexer = new StreamMultiplexer("unimportant", fInput, outputs);
		multiplexer.start();
		multiplexer.join();
		for(Writer output: outputs) {
			output.close();
		}
	}

}
