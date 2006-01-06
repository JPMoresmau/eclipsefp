package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.jparser.LiterateHaskellReader;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

public class LiterateHaskellReaderTest extends TestCase {
	
	private Reader fReader;

	public void testUnlit() throws IOException {
		final String input = "This literate program prompts the user for a number\n" +
							 "and prints the factorial of that number:\n" +
							 "\n" +
							 "> main :: IO ()";
		setReaderInput(input);

		assertRead("\n\n\n");
		assertRead("  main");
	}

	public void testStartWithProgram() throws IOException {
		final String input = "> main :: IO ()\n" +
				             "\n" +
				             "This function calculates the factorial of a\n" +
				             "number\n" +
				             "> fat :: Int -> Int";
		
		setReaderInput(input);

		assertRead("  main :: IO ()\n");
		assertRead("\n\n");
	}
	
	public void testEndOfInput() throws IOException {
		final String input = "> main :: IO ()\n";
		setReaderInput(input);
		
		assertRead("  main :: IO ");
		assertEquals(3, fReader.read(new char[5]));
	}
	
	public void testEmptyInput() throws IOException {
		setReaderInput("");
		
		assertEquals(-1, fReader.read(new char[3]));
	}
	
	public void testTexStyleBeginCode() throws IOException {
		final String input = "\\documentstyle{article}\n" +
				       "\n" +
				       "\\begin{document}\n" +
				       "\n" +
				       "\\section{Introduction}\n" +
				       "\n" +
				       "This is a trivial program that prints the first 20 factorials.\n" +
				       "\n" +
				       "\\begin{code}\n" +
				       "main :: IO ()\n" +
				       "main =  print [ (n, product [1..n]) | n <- [1..20]]\n" +
				       "\\end{code}\n" +
				       "\n" +
				       "\\end{document}";
		setReaderInput(input);
		
		assertRead("\n\n\n\n\n\n\n\n\n");
		assertRead("main :: IO ()\n");
		assertRead("main =  p");
	}
	
	public void testTexStyleEndCode() throws IOException {
		final String input = 
				       "\\begin{document}\n" +
				       "\n" +
				       "\\begin{code}\n" +
				       "main = fat 3\n" +
				       "fat 0 = 1\n" +
				       "\\end{code}\n" +
				       "\n" +
				       "\\end{document}";
		setReaderInput(input);
		
		assertRead("\n\n\n");
		assertRead("main = fat 3\n");
		assertRead("fat 0 = 1\n");
		assertRead("\n\n");
	}
	
	public void testExactBufferSize() throws IOException {
		final String input = "01234567890123456789012345678901234567890123456789012345678901234567\n" +
				             "> fat 0 = 1";
		setReaderInput(input);
		
		assertRead("\n");
		assertRead("  fat 0 = 1");
	}
	
	public void testBufferFill() throws IOException {
		final StringBuffer buf = new StringBuffer(">");
		for(int i = 0; i < 2048; ++i) {
			buf.append('a');
		}
		setReaderInput(buf.toString());
		
		for(int i  = 0; i < 1024 / 79; ++i)
			fReader.read();
		
		final Runnable readBufferBlock = new Runnable() {
			public void run()  {
				try {
					fReader.read();
				} catch (IOException e) {
					//should not happen
					throw new AssertionFailedError();
				}
			}
		};
		
		assertNoDeadLock(readBufferBlock, 5000);
	}
	
	public void test56CharsInsideTextBlock() throws IOException {
		final String program = "\\begin{code}\n" +
		                       "-- This line is exactly 56 characters long (line break e\n" +
		                      //01234567890123456789012345678901234567890123456789012345  
				               "\\end{code}";

		setReaderInput(program);
		assertRead("\n");
		assertRead("-- This line is exactly 56 characters long (line break e\n");
	}

	public void test57CharsInsideTextBlock() throws IOException {
		final String program = "\\begin{code}\n" +
		                       "-- This line is exactly 57 characters long (line break ex\n" +
		                      //012345678901234567890123456789012345678901234567890123456  
				               "\\end{code}";

		setReaderInput(program);
		assertRead("\n");
		assertRead("-- This line is exactly 57 characters long (line break ex\n");
	}

	public void test65CharsInsideTextBlock() throws IOException {
		final String program = "\\begin{code}\n" +
 	                           "--  This line is exactly 65 characters long (line break excluded)\n" +
			                  //01234567890123456789012345678901234567890123456789012345678901234  
				               "\\end{code}";

		setReaderInput(program);
		assertRead("\n");
		assertRead("--  This line is exactly 65 characters long (line break excluded)\n");
	}
	
	public void test66CharsInsideTextBlock() throws IOException {
		final String program = "\\begin{code}\n" +
                               "--  This line is exactly 66  characters long (line break excluded)\n" +
 			                  //012345678901234567890123456789012345678901234567890123456789012345  
                               "\\end{code}\n" +
                               "rasta";

		setReaderInput(program);
		assertRead("\n");
		assertRead("--  This line is exactly 66  characters long (line break excluded)\n");
	}
	
	public void testTexBlockContainsBackslash() throws IOException {
		final String program = "\\begin{code}\n" +
                               "main = putStr \"This is a double-quote: \\\"\"\n" +
                               "\\end{code}";

		setReaderInput(program);
		assertRead("\n");
		assertRead("main = putStr \"This is a double-quote: \\\"\"\n");
	}

	public void testLineInsideTexBlockStartsWithBackslash() throws IOException {
		final String program = "\\begin{code}\n" +
                               "main = putStr \"Hello, \\\n" +
                               "\n" +
                               "\\world!\"\n" +
                               "\\end{code}";

		setReaderInput(program);
		assertRead("\n");
		assertRead("main = putStr \"Hello, \\\n");
		assertRead("\n");
		assertRead("\\world!\"\n");
	}
	
	public void testTwoTexCodeBlocks() throws IOException {
		final String input = "The fat function calculates a factorial\n" +
				             "\\begin{code}\n" +
				             "fat 0 = 1\n" +
				             "fat n = n * (fat (n - 1))\n" +
				             "\\end{code}\n" +
				             "The main function is the program entry point\n" +
				             "\\begin{code}\n" +
				             "main = fat 3\n" +
				             "\\end{code}\n";
		
		setReaderInput(input);
		assertRead("\n\n");
		assertRead("fat 0 = 1\n");
		assertRead("fat n = n * (fat (n - 1))\n");
		assertRead("\n\n\n");
		assertRead("main = fat 3\n");
	}

	private void assertNoDeadLock(Runnable runnable, long timeoutMillis) {
		Thread t = new Thread(runnable);
		t.start();
		try {
			t.join(timeoutMillis);
			if (t.isAlive()) {
				fail("Execution did not finish after " + timeoutMillis + "ms");
			}
		} catch (InterruptedException e) {
			fail("Code interrupted");
		}
	}

	private void assertRead(final String expected) throws IOException {
		char[] readBuffer = new char[expected.length()];
		fReader.read(readBuffer);
		assertEquals(expected, new String(readBuffer));
	}

	private void setReaderInput(final String input) {
		fReader = createReader(input);
	}

	private LiterateHaskellReader createReader(final String input) {
		return new LiterateHaskellReader(new StringReader(input));
	}
	
}