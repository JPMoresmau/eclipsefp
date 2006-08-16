package net.sf.eclipsefp.haskell.core.compiler;

import java.io.IOException;
import java.io.Writer;

/**
 * A writer that ignores its input 
 *
 * @author Thiago Arrais
 */
public class NullWriter extends Writer {

	@Override
	public void close() throws IOException { /* ignore */ }

	@Override
	public void flush() throws IOException { /* ignore */ }

	@Override
	public void write(char[] cbuf, int off, int len) { /* ignore */ }

}
