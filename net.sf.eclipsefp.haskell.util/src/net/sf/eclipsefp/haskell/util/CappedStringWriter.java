/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import java.io.IOException;
import java.io.Writer;
import java.util.LinkedList;

/**
 * A string writer that only keeps n max characters, discarding the first received characters
 * @author JP Moresmau
 *
 */
public class CappedStringWriter extends Writer {
	private int maxSize;
	private LinkedList<String> ls=new LinkedList<>();
	private int size;
	
	public CappedStringWriter(int max){
		maxSize=max;
		lock=ls;
	}
	
	
	/* (non-Javadoc)
	 * @see java.io.Writer#write(char[], int, int)
	 */
	@Override
	public void write(char[] cbuf, int off, int len) throws IOException {
		synchronized (lock) {
			ls.add(new String(cbuf,off,len));
			size+=len;
			checkSize();
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Writer#write(java.lang.String)
	 */
	@Override
	public void write(String str) throws IOException {
		synchronized (lock) {
			if (str!=null){
				ls.add(str);
				size+=str.length();
				checkSize();
			}
		}
	}
	
	/* (non-Javadoc)
	 * @see java.io.Writer#write(java.lang.String, int, int)
	 */
	@Override
	public void write(String str, int off, int len) throws IOException {
		write(str.substring(off, off+len));
	}
	
	private void checkSize(){
		while (size>maxSize){
			String s=ls.removeFirst();
			size-=s.length();
		}
	}
	
	/* (non-Javadoc)
	 * @see java.io.Writer#flush()
	 */
	@Override
	public void flush() throws IOException {
		// noop
	}

	/* (non-Javadoc)
	 * @see java.io.Writer#close()
	 */
	@Override
	public void close() throws IOException {
		// noop

	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		synchronized (lock) {
			StringBuilder sb=new StringBuilder(size);
			for (String s:ls){
				sb.append(s);
			}
			return sb.toString();
		}
	}

	public void clear(){
		synchronized (lock) {
			ls.clear();
			size=0;	
		}
		
	}
}
