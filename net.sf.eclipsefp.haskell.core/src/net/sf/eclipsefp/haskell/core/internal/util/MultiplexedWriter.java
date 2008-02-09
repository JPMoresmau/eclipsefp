/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.core.internal.util;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MultiplexedWriter extends Writer {

	private final List<Writer> fOutputs = new ArrayList<Writer>();

	public MultiplexedWriter(final Writer... outputs) {
		fOutputs.addAll(Arrays.asList(outputs));
	}
	
	public void addOutput(final Writer out) {
		fOutputs.add(out);
	}

	@Override
	public void close() {
		for (Writer output : fOutputs) {
			try {
				output.close();
			} catch (IOException ex) {
				//ignore error for this output and procede to next
			}
		}
	}

	@Override
	public void flush() {
		for (Writer output : fOutputs) {
			try {
				output.flush();
			} catch (IOException ex) {
				//ignore error for this output and procede to next
			}
		}
	}

	@Override
	public void write(final char[] cbuf, final int off, final int len) {
		for (Writer output : fOutputs) {
			try {
				output.write(cbuf, off, len);
			} catch (IOException ex) {
				//ignore error for this output and procede to next
			}
		}
	}

	public void removeOutput(final Writer out) {
		fOutputs.remove(out);
	}

}
