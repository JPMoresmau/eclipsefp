package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import antlr.RecognitionException;
import antlr.TokenStream;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellParser;

import junit.framework.TestCase;

public class ParserTest extends TestCase {
	
	public void testEmptyModule() throws RecognitionException, TokenStreamException {
		parse("module ParserTest where {}");
	}
	
	private void parse(String contents) throws RecognitionException, TokenStreamException {
		TokenStream input = new HaskellLexer(new StringReader(contents));
		HaskellParser parser = new HaskellParser(input);
		
		parser.parseModule();
	}
}
