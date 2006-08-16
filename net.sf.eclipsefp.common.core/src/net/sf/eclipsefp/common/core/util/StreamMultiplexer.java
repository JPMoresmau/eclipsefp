// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.core.util;

import java.io.*;
import java.util.*;

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

	private Reader fInput;

	private List<Writer> fOutputs;

	private Exception ex;

	public StreamMultiplexer(final String name, final InputStream in,
			final Writer... outs) {
		super(name);
		fInput = new InputStreamReader(in);
		fOutputs = Arrays.asList(outs);
		setPriority(Thread.MAX_PRIORITY - 1);
	}

	public Exception getException() {
		return ex;
	}

	// interface methods of java.lang.Thread
	// //////////////////////////////////////

	public void run() {
		try {
			char[] cbuf = new char[BUFFER_SIZE];
			int count;
			while ((count = fInput.read(cbuf, 0, BUFFER_SIZE)) >= 0) {
				for (Writer output : fOutputs) {
					output.write(cbuf, 0, count);
					output.flush();
				}
			}
		} catch (IOException ex) {
			this.ex = ex;
		}
	}
}