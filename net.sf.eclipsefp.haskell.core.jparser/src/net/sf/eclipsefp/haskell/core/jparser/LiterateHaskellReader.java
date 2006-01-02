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
	private int fCurrentExpectedChar = 0;
	private boolean fInsideTexCodeBlock;
	private static final int STANDARD_LINE_LENGTH = 80;
	private static final char[] TEX_BEGIN_STRING = "\\begin{code}\n".toCharArray();

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
			char[] buf = new char[STANDARD_LINE_LENGTH];
			int n = fInput.read(buf);
			for(int i = 0; i < n; ++i) {
				if (fBeginOfLine && !fInsideTexCodeBlock) {
					fProgramLine = isLiterateStartChar(buf[i]);
					++numProcessedChars;
					if (fProgramLine) {
						fProcessedOutput.write(' ');
					} else {
						fProcessedOutput.write('\n');
					}
				} else if (fProgramLine) {
					fProcessedOutput.write(buf[i]);
					++numProcessedChars;
				}
				
				if (!fProgramLine) {
					fProgramLine = fInsideTexCodeBlock = checkTexStyleBegin(buf[i]);
					if (fProgramLine) {
						continue;
					}
				}
				
				fBeginOfLine = buf[i] == '\n';
			} //for
			
			if (n < STANDARD_LINE_LENGTH) {
				fProcessedOutput.close();
				break;
			}
		} //while
		
		return fProcessedInput.read(cbuf, off, len);
	}

	private boolean checkTexStyleBegin(char c) {
		if (c == TEX_BEGIN_STRING[fCurrentExpectedChar]) {
			fCurrentExpectedChar++;
			if (fCurrentExpectedChar == TEX_BEGIN_STRING.length) {
				fCurrentExpectedChar = 0;
				return true;
			}
			return false;
		} else {
			fCurrentExpectedChar = 0;
			return false;
		}
	}

	private boolean isLiterateStartChar(char c) {
		return c == '>';
	}

	@Override
	public void close() throws IOException {
		fInput.close();
	}

}
