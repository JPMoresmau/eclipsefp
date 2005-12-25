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

	private void assertRead(String expected) throws IOException {
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