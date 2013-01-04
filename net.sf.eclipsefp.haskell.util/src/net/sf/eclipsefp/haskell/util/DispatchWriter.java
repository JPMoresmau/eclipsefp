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
	List<Writer> writers=Collections.synchronizedList(new ArrayList<Writer>());
	
	/**
	 * @return the writers
	 */
	public List<Writer> getWriters() {
		return writers;
	}
	
	/* (non-Javadoc)
	 * @see java.io.Writer#write(char[], int, int)
	 */
	@Override
	public void write(char[] cbuf, int off, int len) throws IOException {
		for (Writer w:writers){
			w.write(cbuf,off,len);
		}

	}

	/* (non-Javadoc)
	 * @see java.io.Writer#flush()
	 */
	@Override
	public void flush() throws IOException {
		for (Writer w:writers){
			w.flush();
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Writer#close()
	 */
	@Override
	public void close() throws IOException {
		for (Writer w:writers){
			w.close();
		}
	}

}
