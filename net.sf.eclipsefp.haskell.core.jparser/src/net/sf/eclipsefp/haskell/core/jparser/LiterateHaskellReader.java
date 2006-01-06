package net.sf.eclipsefp.haskell.core.jparser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.Reader;
import java.util.Arrays;

public class LiterateHaskellReader extends Reader {

	private BufferedReader fInput;
	private Reader fProcessedInput ;
	private PipedWriter fProcessedOutput;
	private boolean fBeginOfLine = true;
	private boolean fProgramLine = false;
	private int fCurrentExpectedChar = 0;
	private boolean fInsideTexCodeBlock;
	private static final int STANDARD_LINE_LENGTH = 80;
	private static final char[] TEX_BEGIN_STRING = "\\begin{code}\n".toCharArray();
	private static final char[] TEX_END_STRING = "\\end{code}".toCharArray();

	public LiterateHaskellReader(Reader reader) {
		fInput = new BufferedReader(reader);
		fProcessedOutput = new PipedWriter();
		try {
			fProcessedInput = new BufferedReader(new PipedReader(fProcessedOutput));
		} catch (IOException e) {
			// should not happen
			throw new AssertionError();
		}
	}

	@Override
	public int read(final char[] cbuf, final  int off, final int len) throws IOException {
		int numCharsNeeded = len - countAlreadyAvailableChars(len);
		unlitStream(numCharsNeeded);
		
		return fProcessedInput.read(cbuf, off, len);
	}

	/**
	 * Preprocesses the input stream unliterating it until there are enough
	 * characters. The unlitered output is placed on the piped stream
	 * <code>fProcessedOutput</code> that is connected to the reader
	 * <code>fProcessedInput</code>.
	 * 
	 * @param numCharsNeeded
	 * @throws IOException
	 */
	private void unlitStream(final int numCharsNeeded) throws IOException {
		int numProcessedChars = 0;
		while(numProcessedChars < numCharsNeeded) {
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
				} else if (fInsideTexCodeBlock && buf[i] == TEX_END_STRING[0]) {
					final int endStringSize = TEX_END_STRING.length;
					char[] lookaheadBuffer = new char[endStringSize];
					final int numCharsCopied = n - i > endStringSize ? endStringSize : n - i;
					System.arraycopy(buf, i, lookaheadBuffer, 0, numCharsCopied);
					if (numCharsCopied < endStringSize) {
						fInput.mark(endStringSize - numCharsCopied);
						fInput.read(lookaheadBuffer, numCharsCopied, endStringSize - numCharsCopied);
						fInput.reset();
					}
					fInsideTexCodeBlock = !Arrays.equals(TEX_END_STRING, lookaheadBuffer);
					fProgramLine = fInsideTexCodeBlock;
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
	}

	private int countAlreadyAvailableChars(final int max) throws IOException {
		int numAvailableChars = 0;
		fProcessedInput.mark(max);
		while (fProcessedInput.ready() && numAvailableChars < max) {
			fProcessedInput.read();
			++numAvailableChars;
		}
		fProcessedInput.reset();
		return numAvailableChars;
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
