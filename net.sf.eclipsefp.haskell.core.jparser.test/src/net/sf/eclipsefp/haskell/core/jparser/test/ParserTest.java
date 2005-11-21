package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

import antlr.RecognitionException;
import antlr.TokenStream;
import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.HaskellLexer;
import net.sf.eclipsefp.haskell.core.jparser.HaskellParser;

import junit.framework.TestCase;

/**
 * Tests for the parser componenent.
 * 
 * These tests depend on the HaskellLexer class, it would be good not
 * to do so (although it doesn't hurt too much to do).
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class ParserTest extends TestCase {
	
	public void testEmptyModule() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where {}");

		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEmpty(exports);
		
		IImport[] imports = module.getImports();
		assertNotNull(imports);
		assertEmpty(imports);
		
		IDeclaration[] decls = module.getDeclarations();
		assertNotNull(decls);
		assertEmpty(decls);
	}
	
	public void testUnderlinedModuleName() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Under_lined where {}");
		
		assertNotNull(module);
		
		assertEquals("Under_lined", module.getName());
	}

	public void testModuleWithExports() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest(f1, f2) where {}");

		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(2, exports.length);
		
		assertEquals("f1", exports[0].getName());
		assertEquals("f2", exports[1].getName());
	}
	
	public void testModuleWithNestedBlocks() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest() where {" +
				               "    f = b where { b = 3 } }");

		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(0, exports.length);
	}
	
	//TODO should recognize qualified modules
	
	public void testUntitledModule() throws RecognitionException, TokenStreamException {
		IModule module = parse("{\n" +
				               "fat 0 = 1\n" +
				               "fat n = n * fat (n - 1)\n" +
				               "}");
		
		assertEquals("", module.getName());
	}
	
	public void testOneImport() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where {\n" +
				               "import Library\n" +
				               ";main = putStr 'Hello world!'\n" +
				               "} ");
		
		IImport[] imports = module.getImports();
		assertNotNull(imports);
		assertEquals(1, imports.length);
		
		assertEquals("Library", imports[0].getImportedElement());
	}
	

//TODO should recognize a top level declaration
//	public void testOneTopDeclaration() throws RecognitionException, TokenStreamException {
//		IModule module = parse("module Main where { main = putStr 'Hello world!' }");
//		
//		IDeclaration[] decls = module.getDeclarations();
//		assertNotNull(decls);
//		assertEquals(1, decls.length);
//		
//		assertEquals("main", decls[0].getName());
//	}
	
//TODO should recognize functions with nested blocks

// TODO should recognize more than one declaration
// TODO should recognize import declarations

	private static void assertEmpty(Object[] exports) {
		assertEquals(0, exports.length);
	}
	
	private IModule parse(String contents) throws RecognitionException, TokenStreamException {
		TokenStream input = new HaskellLexer(new StringReader(contents));
		HaskellParser parser = new HaskellParser(input);
		
		return parser.parseModule();
	}
	
	//TODO should keep token location info
}
