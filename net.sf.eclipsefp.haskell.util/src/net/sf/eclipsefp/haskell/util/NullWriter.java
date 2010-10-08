package net.sf.eclipsefp.haskell.util;

import java.io.Writer;

/**
 * A writer that ignores its input 
 *
 * @author Thiago Arrais
 */
public class NullWriter extends Writer {

	@Override
	public void close() { /* ignore */ }

	@Override
	public void flush() { /* ignore */ }

	@Override
	public void write(final char[] cbuf, final int off, final int len) { /* ignore */ }

}
