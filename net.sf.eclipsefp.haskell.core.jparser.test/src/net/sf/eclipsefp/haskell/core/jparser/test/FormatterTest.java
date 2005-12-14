package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import antlr.Token;
import antlr.TokenStream;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellFormatter;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellLexerTokenTypes;

import junit.framework.TestCase;

/**
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class FormatterTest extends TestCase implements HaskellLexerTokenTypes {
	
	private static TestTokenStream createFormatter(final String input) {
		final TokenStream lexer = new HaskellLexer(new StringReader(input));

		return new TestTokenStream(new HaskellFormatter(lexer));
	}
	
	// The sample for these tests was taken from then Haskell Report
	// and is available at
	// http://www.haskell.org/onlinereport/lexemes.html#layout-before

	public void testWhereOpensBlock() throws TokenStreamException {
		final String inStr = "module Simple where\n" +
        					 "data Stack = Empty\n";
		
		final TestTokenStream formatter = createFormatter(inStr);
		
		Token t = formatter.nextToken();
		assertEquals(HaskellLexerTokenTypes.MODULE, t.getType());
		assertEquals("module", t.getText());
		
		t = formatter.nextToken(); //Simple
		t = formatter.nextToken(); //where
		t = formatter.nextToken(); // {
		assertEquals(LEFT_CURLY, t.getType());
		
		t = formatter.nextToken(); //data
		assertEquals("data", t.getText());
		
		t = formatter.nextToken(); //Stack
		t = formatter.nextToken(); //=
		t = formatter.nextToken(); //Empty

		t = formatter.nextToken(); // }
		assertEquals(RIGHT_CURLY, t.getType());
		
		t = formatter.nextToken(); // EOF

		assertEquals(EOF, t.getType());
	}
	
	public void testLetOpensBlock() throws TokenStreamException {
		final String inStr = "{ id x = let b = x\n" +
				             "         in b }";
		
		final TestTokenStream formatter = createFormatter(inStr);
		
		// { id x = let
		formatter.skipTokens(5);
		assertEquals(LEFT_CURLY, formatter.nextToken().getType());
		
		//b = x
		formatter.skipTokens(3);
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
		
		//in b }
		formatter.skipTokens(3);
		assertEquals(EOF, formatter.nextToken().getType());
		
	}
	
	public void testNestedWhere() throws TokenStreamException {
		final String inStr = "module Simple where\n" +
		                     "    id x = a where a = x\n";

		final TestTokenStream formatter = createFormatter(inStr);
		
		//consume 'module Simple where'
		formatter.skipTokens(3);
		assertEquals(LEFT_CURLY, formatter.nextToken().getType());

		//consume 'id x = a where'
		formatter.skipTokens(5);
		assertEquals(LEFT_CURLY, formatter.nextToken().getType());
		
		//consume 'a = x'
		formatter.skipTokens(3);
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
	}
	
	public void testPlaceSemicolon() throws TokenStreamException {
		final String inStr = "module Simple where\n" +
                             "    fat 0 = 1\n" +
                             "    fat x = x * fat (x - 1)\n" +
                             "    id x = x";
		final TestTokenStream formatter = createFormatter(inStr);
		
		//consume 'module Simple where { fat 0 = 1'
		formatter.skipTokens(8);
		assertEquals(SEMICOLON, formatter.nextToken().getType());
		assertEquals("fat", formatter.nextToken().getText());
		
		//consume 'x = x * fat ( x - 1 )'		
		formatter.skipTokens(10);
		assertEquals(SEMICOLON, formatter.nextToken().getType());
		
		//consume 'id x = x'
		formatter.skipTokens(4);
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
		
		assertEquals(EOF, formatter.nextToken().getType());
	}
	
	public void testLayoutIndependentCode() throws TokenStreamException {
		final String inStr = "module Main where { id x = x; main = id 3 }";
		final TestTokenStream formatter = createFormatter(inStr);
		
		//consume 'module Main where'
		formatter.skipTokens(3);
		assertEquals(LEFT_CURLY, formatter.nextToken().getType());
		assertEquals("id", formatter.nextToken().getText());
		
		//consume 'x = x'
		formatter.skipTokens(3);
		assertEquals(SEMICOLON, formatter.nextToken().getType());
		assertEquals("main", formatter.nextToken().getText());
		
		//consume '= id 3'
		formatter.skipTokens(3);
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
		assertEquals(EOF, formatter.nextToken().getType());
	}
	
	public void testWithBraces() throws TokenStreamException {
		final String inStr = "module Main where {\n" +
        					 "    id x = x\n" +
        					 "    main = id 3\n" +
        					 "}";
		
		final TestTokenStream formatter = createFormatter(inStr);
		
		//module Main where {
		//id x = x
		formatter.skipTokens(8);
		
		Token aToken = formatter.nextToken();
		assertEquals(SEMICOLON, aToken.getType());
	}
	
	public void testNoSemicolonAfterLast() throws TokenStreamException {
		final String inStr = "{\n" +
        			  "fat 0 = 1\n" +
                      "fat n = n * fat (n - 1)\n" +
                      "}";
		
		final TestTokenStream formatter = createFormatter(inStr);
		
		// {
		// fat 0 = 1
		// ;
		// fat n = n * fat ( n - 1 )
		formatter.skipTokens(1);
		formatter.skipTokens(4);
		formatter.skipTokens(1);
		formatter.skipTokens(11);
		
		// }
		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
	}
	
	public void testUnnecessaryLayout() throws TokenStreamException {
		final String inStr = "module Main where {\n" +
				             "    id x = x ;\n" +
				             "    main = id 3\n" +
				             "}";
		final TestTokenStream formatter = createFormatter(inStr);
		
		//consume every token on the original stream
		formatter.skipTokens(14);
		
		assertEquals(EOF, formatter.nextToken().getType());
	}
	
	public void testSemicolonAtBeggining() throws TokenStreamException {
		final String inStr = "module Main where {\n" +
        					 "import Library\n" +
                             ";main = putStr 'Hello world!'\n" +
                             "} ";

		final TestTokenStream formatter = createFormatter(inStr);

		//module Main where {
		//import Library
		//;
		formatter.skipTokens(4);
		formatter.skipTokens(2);
		formatter.skipTokens(1);
		
		assertEquals(VARIABLE_ID, formatter.nextToken().getType());
	}
	
	public void testNonStandardCode() throws TokenStreamException {
		final String inStr = "module Test where {\n" +
  							 "#ifdef CURL\n" +
							 "import LibraryCurl.ModuleCurl\n" +
							 "#endif\n" +
							 "foreign export stdcall \"parseHaskellCU\";\n" +
							 "haskellParseCU :: CString -> IO ( StablePtr ( ParseResult HsModule ) );\n" +
							 "}\n";

		final TestTokenStream formatter = createFormatter(inStr);
		
		//module Test where {
		formatter.skipTokens(4);
		
		//# ifdef CURL
		formatter.skipTokens(3);
		
		Token t = formatter.nextToken();
		assertEquals(HaskellLexerTokenTypes.SEMICOLON, t.getType());
	}
	
	public void testUntitledModule() throws TokenStreamException {
		final String inStr = "fat 0 = 1\n" +
				             "fat n = n * (fat (n - 1))";

		final TestTokenStream formatter = createFormatter(inStr);
		
		assertEquals(LEFT_CURLY, formatter.nextToken().getType());
		
		// fat 0 = 1
		// ;
		// fat n = n * ( fat ( n - 1 ) )
		formatter.skipTokens(4);
		formatter.skipTokens(1);
		formatter.skipTokens(13);

		assertEquals(RIGHT_CURLY, formatter.nextToken().getType());
	}
	
	//TODO fix the formatter according to the syntax rules (Haskell Report
	//Ch. 9) Current version implements just the most basic rules
	
	
}
