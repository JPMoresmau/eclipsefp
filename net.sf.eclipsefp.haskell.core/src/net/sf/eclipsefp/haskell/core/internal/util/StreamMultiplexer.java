// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>
 * A thread that copies the contents of an input stream to one or more output
 * streams and terminates when the input stream is finished.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class StreamMultiplexer extends Thread {

	private static final int BUFFER_SIZE = 2048;

	private final Reader fInput;

	private final Writer fOutput;

	public StreamMultiplexer(final String name, final InputStream in,
			final Writer... outs) {
		super(name);
		fInput = new InputStreamReader(in);
		fOutput = new MultiplexedWriter(outs);
		setPriority(Thread.MAX_PRIORITY - 1);
	}

	// interface methods of java.lang.Thread
	// //////////////////////////////////////

	@Override
	public void run() {
		char[] cbuf = new char[BUFFER_SIZE];
		int count;
		try {
			while ((count = fInput.read(cbuf, 0, BUFFER_SIZE)) >= 0) {
				fOutput.write(cbuf, 0, count);
			}
			fOutput.flush();
		} catch (IOException ex) {
			//reading error, abort multiplexing
		}
	}
}