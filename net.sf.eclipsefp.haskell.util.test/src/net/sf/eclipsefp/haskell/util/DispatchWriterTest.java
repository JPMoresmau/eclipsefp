/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;

import junit.framework.TestCase;


/**
 * @author JP Moresmau
 *
 */
public class DispatchWriterTest extends TestCase {

	private static class ProblematicWriter extends Writer {

		@Override
		public void close() throws IOException { }

		@Override
		public void flush() throws IOException { }

		@Override
		public void write(final char[] cbuf, final int off, final int len) throws IOException {
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
		public void write(final char[] cbuf, final int off, final int len) throws IOException {}

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
		DispatchWriter multiplexer = new DispatchWriter();
		multiplexer.getWriters().add(out);
		assertFalse(out.flushed);
		multiplexer.flush();
		assertTrue(out.flushed);

		assertFalse(out.closed);
		multiplexer.close();
		assertTrue(out.closed);
	}

	public void testDoNotMultiplexToRemovedOutput() throws IOException {
		final StringWriter output = new StringWriter();
		DispatchWriter multiplexer = new DispatchWriter();

		multiplexer.getWriters().add(output);
		multiplexer.getWriters().remove(output);
		multiplexer.write("This should not be outputed to the removed writer");

		assertEquals(0, output.toString().length());
	}

	private void multiplexTo(final Writer... outputs) throws InterruptedException, IOException {
		DispatchWriter multiplexer = new DispatchWriter();
		multiplexer.getWriters().addAll(Arrays.asList(outputs));
		multiplexer.write(EXPECTED_CONTENTS);
	}
}

