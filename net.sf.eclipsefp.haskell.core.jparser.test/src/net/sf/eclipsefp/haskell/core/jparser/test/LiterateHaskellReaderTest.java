package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.jparser.LiterateHaskellReader;

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