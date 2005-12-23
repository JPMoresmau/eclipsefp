package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import antlr.Token;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;

import junit.framework.TestCase;

public class LexerTest extends TestCase implements HaskellLexerTokenTypes {
	
	private TestTokenStream fLexer;

	protected void setUp() {
		final String inStr = "module Simple where\n" +
				             "data Underlined_stack = Empty\n";
		
		fLexer = new TestTokenStream(new HaskellLexer(new StringReader(inStr)));
	}

	public void testRecognition() throws TokenStreamException {
		Token t = fLexer.nextToken();
		
		assertEquals(MODULE, t.getType());
		assertEquals("module", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Simple", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(WHERE, t.getType());
		assertEquals("where", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(NEWLINE, t.getType());
		assertEquals("\n", t.getText());

		t = fLexer.nextToken();
		assertEquals(DATA, t.getType());
		assertEquals("data", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Underlined_stack", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(EQUALS, t.getType());
		assertEquals("=", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Empty", t.getText());

		t = fLexer.nextToken();
		assertEquals(NEWLINE, t.getType());
		assertEquals("\n", t.getText());

		t = fLexer.nextToken();
		assertEquals(EOF, t.getType());
	}
	
	public void testRecognizeLet() throws TokenStreamException {
		fLexer = createLexer("let in");
		assertEquals(LET, fLexer.nextToken().getType());
	}
	
	public void testCommonPrefixes() throws TokenStreamException {
		fLexer = createLexer("main whomp modula whery");
		
		Token t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("main", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("whomp", t.getText());

		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("modula", t.getText());

		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("whery", t.getText());
	}
	
	public void testKeyworkPreffixInsideIdentifier() throws TokenStreamException {
		fLexer = createLexer("Pwho imodule");
		
		Token t = fLexer.nextToken();
		assertEquals(CONSTRUCTOR_ID, t.getType());
		assertEquals("Pwho", t.getText());
		
		t = fLexer.nextToken();
		assertEquals(VARIABLE_ID, t.getType());
		assertEquals("imodule", t.getText());
	}
	
	public void testPosition() throws TokenStreamException {
		Token t = fLexer.nextToken(); //module
		assertEquals(0, t.getColumn());
		assertEquals(0, t.getLine());
		
		t = fLexer.nextToken(); // Simple
		assertEquals(7, t.getColumn());
		assertEquals(0, t.getLine());
		
		t = fLexer.nextToken(); // where
		assertEquals(0, t.getLine());
	
		t = fLexer.nextToken(); // \n

		t = fLexer.nextToken();
		assertEquals(0, t.getColumn());
		assertEquals(1, t.getLine());

		t = fLexer.nextToken();
		assertEquals(5, t.getColumn());
		assertEquals(1, t.getLine());
	}
	
	public void testCodeWithComments() throws TokenStreamException {
		fLexer = createLexer("--this is the main module for the app\n" +
				             "module Main where\n" +
				             "{- We actually need to import those\n" +
				             "   modules here for using the network\n" +
				             "   connection capabilities -}\n" +
				             "import Network\n" +
				             "\n" +
				             "main = {- block comment inside -} putStr 'hello'\n"
				             );
		Token t = fLexer.nextToken();
		assertEquals(COMMENT, t.getType());
		assertEquals("--this is the main module for the app\n", t.getText());
		
		t = fLexer.nextToken(); //module
		assertEquals(0, t.getColumn());
		assertEquals(1, t.getLine());
		
		fLexer.skipTokens(3); //Main where \n
		
		t = fLexer.nextToken();
		assertEquals(COMMENT, t.getType());
		assertEquals("{- We actually need to import those\n" +
	                 "   modules here for using the network\n" +
	                 "   connection capabilities -}",
	                 t.getText());
		
		fLexer.skipTokens(1); // \n

		t = fLexer.nextToken(); //import
		assertEquals(5, t.getLine());
		
		fLexer.skipTokens(5); //Network \n \n main = 
		
		t = fLexer.nextToken(); // {- block comment inside -}
		assertEquals(COMMENT, t.getType());
		assertEquals("{- block comment inside -}", t.getText());
		
		t = fLexer.nextToken(); //putStr
		assertEquals("putStr", t.getText());
		assertEquals(34, t.getColumn());
	}
	
	public void testSimpleStringLiteral() throws TokenStreamException {
		final String input = "main = putStr \"Hello, world!\"";
		fLexer = createLexer(input);
		
		// main = putStr
		fLexer.skipTokens(3);
		
		Token helloWorldTk = fLexer.nextToken();
		assertEquals(STRING_LITERAL, helloWorldTk.getType());
		assertEquals("Hello, world!", helloWorldTk.getText());
	}
	
	public void testMultilineString() throws TokenStreamException {
		final String input = "main = putStr \"Hello, \\\n" +
				             "                  \\world!\"";
		fLexer = createLexer(input);
		
		// main = putStr
		fLexer.skipTokens(3);
		
		Token helloWorldTk = fLexer.nextToken();
		assertEquals(STRING_LITERAL, helloWorldTk.getType());
		assertEquals("Hello, world!", helloWorldTk.getText());
	}
	
	private TestTokenStream createLexer(String input) {
		return new TestTokenStream(new HaskellLexer(new StringReader(input)));
	}
	
	//TODO maybe we need to recognize more comment formats. the report
	//specifies a return char as the end of a line comment too

	public void testStringWithEscapeChar() throws TokenStreamException {
		final String input = "main = putStr \"Hello, world!\\n\" " +
				             "\"tab\\t\" \"slash\\\\\" \"double quote\\\"\" " +
				             "\"quote\\'\" \"backspace\\b\" \"alert\\a\" " +
				             "\"formfeed\\f\" \"return\\r\" \"vertical tab\\v\" " +
				             "\"null \\&char\"";
		fLexer = createLexer(input);
		
		// main = putStr
		fLexer.skipTokens(3);
		
		assertToken(STRING_LITERAL, "Hello, world!\n", fLexer.nextToken());
		assertToken(STRING_LITERAL, "tab\t", fLexer.nextToken());
		assertToken(STRING_LITERAL, "slash\\", fLexer.nextToken());
		assertToken(STRING_LITERAL, "double quote\"", fLexer.nextToken());
		assertToken(STRING_LITERAL, "quote'", fLexer.nextToken());
		assertToken(STRING_LITERAL, "backspace\b", fLexer.nextToken());
		assertToken(STRING_LITERAL, "alert", fLexer.nextToken());
		assertToken(STRING_LITERAL, "formfeed\f", fLexer.nextToken());
		assertToken(STRING_LITERAL, "return\r", fLexer.nextToken());
		assertToken(STRING_LITERAL, "vertical tab", fLexer.nextToken());
		assertToken(STRING_LITERAL, "null char", fLexer.nextToken());
	}

	public void testCharacterLiteral() throws TokenStreamException {
		final String input = "'a' 'b' 'Z' '\\n'";
		fLexer = createLexer(input);
		
		assertToken(CHARACTER_LITERAL, "a", fLexer.nextToken());
		assertToken(CHARACTER_LITERAL, "b", fLexer.nextToken());
		assertToken(CHARACTER_LITERAL, "Z", fLexer.nextToken());
		assertToken(CHARACTER_LITERAL, "\n", fLexer.nextToken());
	}
	
	public void testDoNotAcceptNullCharacter() {
		final String input = "'\\&'";
		fLexer = createLexer(input);
		
		try {
			fLexer.nextToken();
			fail("lexer accepted null character");
		} catch(TokenStreamException e) {
			// exception is expected
		}
	}
	
	public void testEscapeAscii() throws TokenStreamException {
		final String input = "'\\NUL'";
		fLexer = createLexer(input);
		
		assertToken(CHARACTER_LITERAL, "\u0000", fLexer.nextToken());
	}
	
	public void testEspaceDecimal() throws TokenStreamException {
		final String input = "'\\31' '\\139'";
		fLexer = createLexer(input);
		
		assertToken(CHARACTER_LITERAL, "" + ((char) 31), fLexer.nextToken());
		assertToken(CHARACTER_LITERAL, "" + ((char) 139), fLexer.nextToken());
	}
	
	public void testIgnorePreprocessor() throws TokenStreamException {
		final String input = "#ifdef HAVE_CURL\n" +
				             "import Foreign.C.String ( withCString, CString )\n" +
				             "#endif";
		
		fLexer = createLexer(input);
		final Token impToken = fLexer.nextToken();
		assertTokenType(IMPORT, impToken);
		assertEquals(1, impToken.getLine());
	}

	private void assertTokenType(int expectedType, Token token) {
		assertEquals(expectedType, token.getType());
	}

	private void assertToken(int expectedType, String expectedText, Token token) {
		assertEquals(expectedType, token.getType());
		assertEquals(expectedText, token.getText());
	}
	
	// TODO escape  -> 	 \ ( charesc | ascii | decimal | o octal | x hexadecimal )
	//      charesc -> 	a | b | f | n | r | t | v | \ | " | ' | &
	
	//TODO scan literate haskell (maybe this doesn't even mess with the lexer)
	//take a look at the Language.Haskell.Parser impl
	
	// TODO identifiers may include single quotes
	
	// TODO look at the qualified name examples at the 2.4 section of the report
}
