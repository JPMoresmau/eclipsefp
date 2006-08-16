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

	public StreamMultiplexer(final String name, final InputStream in,
			final Writer... outs) {
		super(name);
		fInput = new InputStreamReader(in);
		fOutputs = Arrays.asList(outs);
		setPriority(Thread.MAX_PRIORITY - 1);
	}

	// interface methods of java.lang.Thread
	// //////////////////////////////////////

	public void run() {
		char[] cbuf = new char[BUFFER_SIZE];
		int count;
		try {
			while ((count = fInput.read(cbuf, 0, BUFFER_SIZE)) >= 0) {
				for (Writer output : fOutputs) {
					try {
						output.write(cbuf, 0, count);
					} catch (IOException ex) {
						//ignore error for this output and procede to next
					}
				}
			}
		} catch (IOException ex) {
			//reading error, abort multiplexing
		}
	}
}