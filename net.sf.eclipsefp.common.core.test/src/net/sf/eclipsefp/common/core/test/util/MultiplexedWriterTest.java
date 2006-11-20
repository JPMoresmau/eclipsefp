package net.sf.eclipsefp.common.core.test.util;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import net.sf.eclipsefp.common.core.util.MultiplexedWriter;
import junit.framework.TestCase;

public class MultiplexedWriterTest extends TestCase {

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
	
	private static class StubWriter extends Writer {

		boolean closed;
		boolean flushed;

		@Override
		public void close() throws IOException { closed = true; }

		@Override
		public void flush() throws IOException { flushed = true; }

		@Override
		public void write(char[] cbuf, int off, int len) throws IOException {}
		
	}

	private final String EXPECTED_CONTENTS = "expected contents";

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
	
	public void testFlushesAndClosesOutputs() throws IOException {
		final StubWriter out = new StubWriter();
		Writer multiplexer = new MultiplexedWriter(out);
		assertFalse(out.flushed);
		multiplexer.flush();
		assertTrue(out.flushed);
		
		assertFalse(out.closed);
		multiplexer.close();
		assertTrue(out.closed);
	}
	
	public void testDoNotMultiplexToRemovedOutput() throws IOException {
		final StringWriter output = new StringWriter();
		MultiplexedWriter multiplexer = new MultiplexedWriter();
		
		multiplexer.addOutput(output);
		multiplexer.removeOutput(output);
		multiplexer.write("This should not be outputed to the removed writer");

		assertEquals(0, output.toString().length());
	}

	private void multiplexTo(Writer... outputs) throws InterruptedException, IOException {
		Writer multiplexer = new MultiplexedWriter(outputs);
		multiplexer.write(EXPECTED_CONTENTS);
	}
}
