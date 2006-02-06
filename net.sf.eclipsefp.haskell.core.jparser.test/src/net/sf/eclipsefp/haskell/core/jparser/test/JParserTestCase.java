package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.Reader;
import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.jparser.HaskellParser;
import antlr.RecognitionException;
import antlr.TokenStreamException;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import junit.framework.TestCase;

/**
 * Convenience class for tests that use the parser. Inheritance can be used
 * to include the <code>parse</code> methods on a child class or used
 * as a library class  by calling them statically.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class JParserTestCase extends TestCase {

	public static IModule parse(String contents) throws RecognitionException, TokenStreamException {
		return parse(new StringReader(contents));
	}

	public static IModule parse(final Reader input) throws RecognitionException, TokenStreamException {
		HaskellParser parser = new HaskellParser(input);
		
		return parser.parseModule();
	}
	
}
