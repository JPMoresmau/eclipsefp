package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import de.leiffrenzel.fp.haskell.core.halamo.IClassDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IDataDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.INewTypeDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeDeclaration;

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
		IModule module = parse("module ParserTest(f1, f2, f3.f4) where {}");

		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(3, exports.length);
		
		assertEquals("f1", exports[0].getName());
		assertEquals("f2", exports[1].getName());
		assertEquals("f3.f4", exports[2].getName());
	}
	
	public void testExportingTypeConstructors() throws RecognitionException, TokenStreamException {
		//module declaration borrowed from darcs source code
		IModule module = parse("module Curl ( copyUrl, " +
				                             "Cachable(Cachable, " +
				                                      "Uncachable, " +
				                                      "MaxAge) )" +
				               "where {}");
		
		assertNotNull(module);
		assertEquals("Curl", module.getName());

		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(2, exports.length);
		
		assertEquals("copyUrl", exports[0].getName());
		assertEquals("Cachable", exports[1].getName());
	}
	
	public void testExportingTypesWithAllConstructors() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest ( Stack(..) ) where {}");
		
		assertNotNull(module);
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(1, exports.length);
		
		assertNotNull(exports[0]);
		assertEquals("Stack", exports[0].getName());
	}
	
	public void testExportingQualifiedTypes() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest ( ModuleM.TypeT ) where {}");
		
		assertEquals("ModuleM.TypeT", module.getExportSpecifications()[0].getName());
	}
	
	public void testExportingTypesWithoutConstructors() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest ( Stack ) where {}");
		
		assertNotNull(module);
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(1, exports.length);
		
		assertNotNull(exports[0]);
		assertEquals("Stack", exports[0].getName());
	}
	
	public void testExportingTypeClasses() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest ( TypeClass (these, are, " +
				                                              "some, class, " +
				                                              "operations ))" +
				               "where {}");
		
		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(1, exports.length);
		
		assertNotNull(exports[0]);
		assertEquals("TypeClass", exports[0].getName());
	}

	public void testExportingModule() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest( module M ) where {" +
        					   "    f = b where { b = 3 } }");
		
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertEquals(1, exports.length);
		assertEquals("M", exports[0].getName());
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
	
	public void testSelectiveImports() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
							   "\n" +
				               "    import ModuleM ( funF, funG )\n" +
				               "    import ModuleN hiding ( funA , )");
		
		IImport[] imports = module.getImports();
		assertNotNull(imports);
		assertEquals(2, imports.length);
		
		assertEquals("ModuleM", imports[0].getName());
		assertEquals("ModuleN", imports[1].getName());
	}
	
	public void testImportingTypeConstructors() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
				   "\n" +
	               "    import ModuleM ( TypeT(..), TypeU( ConC, ConD ), TypeV )\n");
		
		assertEquals("Main", module.getName());
		
		assertEquals("ModuleM", module.getImports()[0].getName());
	}
	
	public void testImportingTypeClasses() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
				   "\n" +
	               "    import ModuleM ( TypeC(..), TypeU( funF, funG ), TypeV )\n");
		
		assertEquals("Main", module.getName());
		
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
	
	public void testOneFunctionDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where { main = putStr 'Hello world!' }");
		
		IDeclaration[] decls = module.getDeclarations();
		assertNotNull(decls);
		assertEquals(1, decls.length);
		
		assertEquals("main", decls[0].getName());
		assertTrue(decls[0] instanceof IFunctionBinding);
	}
	
	public void testMultipleFunctionDeclarations() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where { main = writeout ; writeout = putStr 'Hello world!' }");
	
		IDeclaration[] decls = module.getDeclarations();
		
		assertTrue(decls[0] instanceof IFunctionBinding);
		assertTrue(decls[1] instanceof IFunctionBinding);
		
		assertEquals("main", decls[0].getName());
		assertEquals("writeout", decls[1].getName());
	}
	
	public void testFunctionWithWhereBlock() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
				               "    main = writeout\n" +
				               "      where writeout = putStr 'Hello world!'");
		
		IDeclaration[] decls = module.getDeclarations();
		
		assertEquals(1, decls.length);
		assertEquals("main", decls[0].getName());
	}
	
	public void testFunctionWithLetBlock() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
					           "    main = let writeout = putStr 'Hello world!' in\n" +
		                       "      writeout\n" +
		                       "    fun = 3");
		
		IDeclaration[] decls = module.getDeclarations();
		
		assertEquals(2, decls.length);
		assertEquals("main", decls[0].getName());
		assertEquals("fun", decls[1].getName());
	}
	
	public void testFunctionWithParameters() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
		                       "    main = fat 3\n" +
                               "    fat 0 = 1\n" +
                               "    fat n = n * (fat (n - 1))");

		IDeclaration[] decls = module.getDeclarations();
		
		assertEquals(3, decls.length);
		assertEquals("main", decls[0].getName());
		assertEquals("fat", decls[1].getName());
		assertEquals("fat", decls[2].getName());
	}
	
	public void testTypeSynonymDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
   							   "    type Name = [Char]\n" +
			                   "    main = putStr 'Hello, world!'\n");
		
		IDeclaration[] decls = module.getDeclarations();
		
		assertTrue(decls[0] instanceof ITypeDeclaration);
		assertEquals("Name", decls[0].getName());
		
		assertEquals("main", decls[1].getName());
	}
	
	public void testAlgebraicDatatypeDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
				                "    data DataType = ConC | ConD\n" +
				                "    c = ConC");
		
		assertEquals("ParserTest", module.getName());
		
		IDeclaration[] decls = module.getDeclarations();
		
		assertTrue(decls[0] instanceof IDataDeclaration);
		assertEquals("DataType", decls[0].getName());
	}
	
	public void testRenamedDatatypeDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
                			   "    newtype Name = Name [Char]\n");
		
		assertEquals("ParserTest", module.getName());

		IDeclaration[] decls = module.getDeclarations();
		
		assertTrue(decls[0] instanceof INewTypeDeclaration);
		assertEquals("Name", decls[0].getName());
	}
	
	public void testDatatypeWithContextDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
                               "    data Eq t => Algebraic = ConC t | ConD\n" +
		                       "    newtype Eq t => Renamed = Name [Char]\n");
		
		assertEquals("ParserTest", module.getName());
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals("Algebraic", decls[0].getName());
		assertEquals("Renamed", decls[1].getName());
	}
	
	public void testSimpleClassDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
				               "    class VerySimple v\n" +
                               "    class  Eq a  where\n" +
                               "        (==), (/=)  ::  a -> a -> Bool\n" +
                               "\n" +
                               "        x /= y  = not (x == y)" +
                               "        x == y  = not (x /= y)");
		
		assertEquals("ParserTest", module.getName());
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals("VerySimple", decls[0].getName());
		assertTrue(decls[0] instanceof IClassDeclaration);
		
		assertEquals("Eq", decls[1].getName());
		assertTrue(decls[1] instanceof IClassDeclaration);
		
	}
	
	public void testContextClassDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
                               "  class  (Eq a) => Ord a  where\n" +
                               "    compare              :: a -> a -> Ordering\n" +
                               "    (<), (<=), (>=), (>) :: a -> a -> Bool\n" +
                               "    max, min             :: a -> a -> a");
		
		assertEquals("ParserTest", module.getName());
		IDeclaration classDecl = module.getDeclarations()[0];
		assertEquals("Ord", classDecl.getName());
		assertTrue(classDecl instanceof IClassDeclaration);
	}
	
//	public void testSimpleInstanceDeclaration() throws RecognitionException, TokenStreamException {
//		IModule module = parse("module ParserTest where\n" +
//				               "  instance (Eq a, Show a) => Foo [a]\n" +
//				               "  instance Functor a => Bar [a] where {}\n");
//		
//		assertEquals("ParserTest", module.getName());
//
//	}
	
//TODO a topdecl can be one of the following
//	| 	instance [scontext =>] qtycls inst [where idecls]
//	| 	default (type1 , ... , typen) 	(n>=0)
//	| 	decl
	
//TODO try to declare the function '(==) a b = not (a /= b)'
	
	private static void assertEmpty(Object[] exports) {
		assertEquals(0, exports.length);
	}
	
	private IModule parse(String contents) throws RecognitionException, TokenStreamException {
		HaskellParser parser = new HaskellParser(new StringReader(contents));
		
		return parser.parseModule();
	}
	
	//TODO should be able to build a tree with a partial parse, not just
	//with valid inputs
	
	//TODO pay attention to the varsym rules (they can appear anywhere a
	//normal var appears)
	
}
