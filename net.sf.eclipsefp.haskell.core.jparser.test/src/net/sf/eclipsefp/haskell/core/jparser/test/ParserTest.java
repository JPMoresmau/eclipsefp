package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

import antlr.RecognitionException;
import antlr.TokenStreamException;

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
	
	public void testQualifiedModuleName() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Parser.Test() where {}");

		assertNotNull(module);
		assertEquals("Parser.Test", module.getName());
	}
	
	
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
	
	public void testMultipleImports() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where {\n" +
	               "import LibraryA;\n" +
	               "import LibraryB.ModuleM;\n" +
	               "main = putStr 'Hello world!'\n" +
	               "} ");

		IImport[] imports = module.getImports();
		assertNotNull(imports);
		assertEquals(2, imports.length);
		
		assertEquals("LibraryA", imports[0].getImportedElement());
		assertEquals("LibraryB.ModuleM", imports[1].getImportedElement());
	}
	
	public void testImportLocationRecording() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where {\n" +
				               "import LibraryA;\n" +
				               "import LibraryB.ModuleM;\n" +
				               "\n" +
				               "import LibraryC.ModuleN;\n" +
				               "main = putStr 'Hello world!'\n" +
				               "} ");

		IImport[] imports = module.getImports();
		assertNotNull(imports);
		assertEquals(3, imports.length);
		
		assertNotNull(imports[0].getSourceLocation());
		assertEquals(1, imports[0].getSourceLocation().getLine());
		
		assertNotNull(imports[1].getSourceLocation());
		assertEquals(2, imports[1].getSourceLocation().getLine());

		assertNotNull(imports[2].getSourceLocation());
		assertEquals(4, imports[2].getSourceLocation().getLine());
	}
	
	public void testAliasedImport() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
							   "\n" +
				               "    import ModuleM as M");
		
		assertEquals("ModuleM", module.getImports()[0].getName());
	}
	
	public void testOnlyImports() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
							   "\n" +
				               "    import LibraryL.ModuleM\n" +
				               "    import LibraryL.ModuleN\n");
		
		IImport[] imports = module.getImports();
		assertNotNull(imports);
		assertEquals(2, imports.length);
	}
	
	public void testWithComments() throws RecognitionException, TokenStreamException {
		IModule module = parse( "--this is the main module for the app\n" +
						        "module Main where\n" +
						        "{- We actually need to import those\n" +
						        "   modules here for using the network\n" +
						        "   connection capabilities -}\n" +
						        "import Network\n" +
						        "\n" +
						        "main = {- block comment inside -} putStr 'hello'\n");
		
		assertEquals(5, module.getImports()[0].getSourceLocation().getLine());
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

	private static void assertEmpty(Object[] exports) {
		assertEquals(0, exports.length);
	}
	
	private IModule parse(String contents) throws RecognitionException, TokenStreamException {
		HaskellParser parser = new HaskellParser(new StringReader(contents));
		
		return parser.parseModule();
	}
	
	//TODO should be able to build a tree with a partial parse, not just
	//with valid inputs
	
}
