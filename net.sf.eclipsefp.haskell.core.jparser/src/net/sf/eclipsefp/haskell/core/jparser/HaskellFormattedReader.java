package net.sf.eclipsefp.haskell.core.jparser;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.Arrays;

public class HaskellFormattedReader extends Reader {

	public HaskellFormattedReader(Reader in) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public int read(char[] cbuf, int off, int len) throws IOException {
		Arrays.fill(cbuf, off, off + len - 2, 'a');
		cbuf[off + len -1] = '\n';
		return len;
	}

	@Override
	public void close() throws IOException {
		// TODO Auto-generated method stub

	}

}
