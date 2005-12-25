package net.sf.eclipsefp.haskell.core.jparser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.Reader;

public class LiterateHaskellReader extends Reader {

	private BufferedReader fInput;
	private PipedReader fProcessedInput ;
	private PipedWriter fProcessedOutput;
	private boolean fBeginOfLine = true;
	private boolean fProgramLine = false; 

	public LiterateHaskellReader(Reader reader) {
		fInput = new BufferedReader(reader);
		fProcessedInput = new PipedReader();
		try {
			fProcessedOutput = new PipedWriter(fProcessedInput);
		} catch (IOException e) {
			// should not happen
			throw new AssertionError();
		}
	}

	@Override
	public int read(final char[] cbuf, final  int off, final int len) throws IOException {
		int numProcessedChars = 0;
		while(numProcessedChars < len) {
			char[] buf = new char[80];
			int n = fInput.read(buf);
			for(int i = 0; i < n; ++i) {
				if (fBeginOfLine) {
					fProgramLine = buf[i] == '>';
					if (fProgramLine) {
						fProcessedOutput.write(' ');
						++numProcessedChars;
					} else {
						fProcessedOutput.write('\n');
						++numProcessedChars;
					}
				} else if (fProgramLine) {
					fProcessedOutput.write(buf[i]);
					++numProcessedChars;
				}
				
				fBeginOfLine = buf[i] == '\n';
			}
			
			if (n < 80) {
				break;
			}
		}
		
		return fProcessedInput.read(cbuf, off, len);
	}

	@Override
	public void close() throws IOException {
		fInput.close();
	}

}
