/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Dispatch write operations to several writers
 * @author JP Moresmau
 *
 */
public class DispatchWriter extends Writer {
	private final List<Writer> writers=Collections.synchronizedList(new ArrayList<Writer>());
	private final List<Writer> fClosedOutputs = new ArrayList<Writer>();
	
	/**
	 * @return the writers
	 */
	public List<Writer> getWriters() {
		return writers;
	}

	private void outputClosed(final Writer output) {
		  fClosedOutputs.add(output);
		}

	private void removeClosedOutputs() {
	  for (Writer output : fClosedOutputs) {
	    writers.remove( output );
	  }
	  fClosedOutputs.clear();
	}

	
	/* (non-Javadoc)
	 * @see java.io.Writer#write(char[], int, int)
	 */
	@Override
	public void write(char[] cbuf, int off, int len) {
		for (Writer w:writers){
			try {
				w.write(cbuf,off,len);
			} catch (IOException ex) {
				outputClosed(w);
			}
		}
		removeClosedOutputs();
	}

	/* (non-Javadoc)
	 * @see java.io.Writer#flush()
	 */
	@Override
	public void flush()  {
		for (Writer w:writers){
			try {
				w.flush();
			} catch (IOException ex) {
				outputClosed(w);
			}	
		}
		
		removeClosedOutputs();
	}

	/* (non-Javadoc)
	 * @see java.io.Writer#close()
	 */
	@Override
	public void close() {
		for (Writer w:writers){
			try {
				w.close();
			} catch (IOException ex) {
				outputClosed(w);
			}	
		}
		removeClosedOutputs();
	}

}
