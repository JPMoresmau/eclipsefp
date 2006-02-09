package net.sf.eclipsefp.haskell.core.jparser.test;


import java.io.PrintStream;

import net.sf.eclipsefp.haskell.core.jparser.test.doubles.MockPrintStream;
import de.leiffrenzel.fp.haskell.core.halamo.IClassDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IConstructor;
import de.leiffrenzel.fp.haskell.core.halamo.IDataDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IDefaultDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IExportThingAll;
import de.leiffrenzel.fp.haskell.core.halamo.IExportThingWith;
import de.leiffrenzel.fp.haskell.core.halamo.IExportThingWithComponent;
import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IImportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IInfixDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IInstanceDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.INewTypeDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IPatternBinding;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeSignature;

import antlr.RecognitionException;
import antlr.TokenStreamException;


/**
 * Tests for the parser componenent.
 * 
 * These tests depend on the HaskellLexer class, it would be good not
 * to do so (although it doesn't hurt too much to do).
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class ParserIntegrationTest extends JParserTestCase {
	
	private PrintStream fOldErrStream;

	@Override
	protected void setUp() throws Exception {
		fOldErrStream = System.err;
		MockPrintStream mockErr = new MockPrintStream(fOldErrStream);
		
		System.setErr(mockErr);
	}

	public void testEmptyModule() throws RecognitionException, TokenStreamException {
		IModule module = parse("\n   module ParserTest where {}");

		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		assertEquals(1, module.getSourceLocation().getLine());
		assertEquals(3, module.getSourceLocation().getColumn());
		assertEquals(4, module.getSourceLocation().getOffset());
		
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

	public void testModuleWithOneExport() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest(f1) where {}");

		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(1, exports.length);
		assertEquals("f1", exports[0].getName());
	}

	public void testModuleWithExports() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest(f1, f2, M1.f4) where {}");

		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(3, exports.length);
		
		assertEquals("f1", exports[0].getName());
		assertEquals("f2", exports[1].getName());
		assertEquals("M1.f4", exports[2].getName());
	}
	
	public void testExportWithOptionalComma() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest(f1, f2,) where {}");

		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(2, exports.length);
		
		assertEquals("f1", exports[0].getName());
		assertEquals("f2", exports[1].getName());
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
		
		assertTrue(exports[1] instanceof IExportThingWith);
		
		IExportThingWith cacheableExp = (IExportThingWith) exports[1];
		IExportThingWithComponent[] expComponents = cacheableExp.getComponents();
		
		assertNotNull(expComponents);
		assertEquals(3, expComponents.length);
		assertEquals("Cachable", expComponents[0].getName());
		assertEquals("Uncachable", expComponents[1].getName());
		assertEquals("MaxAge", expComponents[2].getName());
	}
	
	public void testExportingTypesWithAllConstructors() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest ( Stack(..) ) where {}");
		
		assertNotNull(module);
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(1, exports.length);
		
		assertNotNull(exports[0]);
		assertTrue(exports[0] instanceof IExportThingAll);
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
				                                              "some, clazz, " +
				                                              "operations ))" +
				               "where {}");
		
		assertNotNull(module);
		assertEquals("ParserTest", module.getName());
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertNotNull(exports);
		assertEquals(1, exports.length);
		
		assertNotNull(exports[0]);
		assertEquals("TypeClass", exports[0].getName());
		
		assertTrue(exports[0] instanceof IExportThingWith);

		IExportThingWith cacheableExp = (IExportThingWith) exports[0];
		IExportThingWithComponent[] expComponents = cacheableExp.getComponents();
		
		assertNotNull(expComponents);
		assertEquals(5, expComponents.length);
		assertEquals("these", expComponents[0].getName());
		assertEquals("are", expComponents[1].getName());
		assertEquals("some", expComponents[2].getName());
		assertEquals("clazz", expComponents[3].getName());
		assertEquals("operations", expComponents[4].getName());
		
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
				               ";main = putStr \"Hello world!\"\n" +
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
	               "main = putStr \"Hello world!\"\n" +
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
				               "main = putStr \"Hello world!\"\n" +
				               "} ");

		IImport[] imports = module.getImports();
		assertNotNull(imports);
		assertEquals(3, imports.length);
		
		assertNotNull(imports[0].getSourceLocation());
		assertEquals(1, imports[0].getSourceLocation().getLine());
		assertEquals(20, imports[0].getSourceLocation().getOffset());
		
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
		assertTrue(imports[1].isHiding());
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
	
	public void testImportingComponents() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
				   "\n" +
	               "import Data.List (unzip3, unzip4, unzip5, unzip6, unzip7)");
		
		final IImport imp = module.getImports()[0];
		final IImportSpecification[] impSpecs = imp.getImportSpecifications();

		assertEquals(5, impSpecs.length);
		assertEquals("unzip3", impSpecs[0].getName());
		assertEquals("unzip5", impSpecs[2].getName());
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
						        "main = {- block comment inside -} putStr \"hello\"\n");
		
		assertEquals(5, module.getImports()[0].getSourceLocation().getLine());
		assertEquals(160, module.getImports()[0].getSourceLocation().getOffset());
	}
	
	public void testOneFunctionDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where {\n" +
				               "    main = putStr \"Hello world!\" }");
		IDeclaration[] decls = module.getDeclarations();
		assertNotNull(decls);
		assertEquals(1, decls.length);
		
		assertEquals("main", decls[0].getName());
		assertTrue(decls[0] instanceof IFunctionBinding);
		
		ISourceLocation srcLoc = decls[0].getSourceLocation();
		assertEquals(1, srcLoc.getLine());
		assertEquals(4, srcLoc.getColumn());
	}
	
	public void testMultipleFunctionDeclarations() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where { main = writeout ; writeout = putStr \"Hello world!\" }");
	
		IDeclaration[] decls = module.getDeclarations();
		
		assertTrue(decls[0] instanceof IFunctionBinding);
		assertTrue(decls[1] instanceof IFunctionBinding);
		
		assertEquals("main", decls[0].getName());
		assertEquals("writeout", decls[1].getName());
	}
	
	public void testFunctionWithWhereBlock() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
				               "    main = writeout\n" +
				               "      where writeout = putStr \"Hello world!\"");
		
		IDeclaration[] decls = module.getDeclarations();
		
		assertEquals(1, decls.length);
		assertEquals("main", decls[0].getName());
	}
	
	public void testFunctionWithLetBlock() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
					           "    main = let writeout = putStr \"Hello world!\" in\n" +
		                       "      writeout\n" +
		                       "    fun = 3");
		
		IDeclaration[] decls = module.getDeclarations();
		
		assertEquals(2, decls.length);
		assertEquals("main", decls[0].getName());
		assertTrue(decls[1] instanceof IPatternBinding);
		assertEquals("fun", decls[1].getName());
	}
	
	public void testFunctionWithParameters() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
		                       "    main = fat 3\n" +
                               "    id x = x");

		IDeclaration[] decls = module.getDeclarations();
		
		assertEquals(2, decls.length);
		assertEquals("main", decls[0].getName());
		assertEquals("id", decls[1].getName());
	}
	
	public void testGroupingMatches() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
  			                   "    main = fat 3\n" +
				               "    fat 0 = 1\n" +
				               "    fat n = n * (fat (n - 1))");
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertEquals("main", decls[0].getName());
		assertEquals("fat", decls[1].getName());
	}
	
	public void testTypeSynonymDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module Main where\n" +
   							   "    type Name = [Char]\n" +
			                   "    main = putStr \"Hello, world!\"\n");
		
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
		
		final IDataDeclaration dataDecl = (IDataDeclaration) decls[0];
		final IConstructor[] constrs = dataDecl.getConstructors();
		assertEquals(2, constrs.length);
		assertEquals("ConC", constrs[0].getName());
		assertEquals("ConD", constrs[1].getName());
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
	
	public void testDataLocationRecording() throws RecognitionException, TokenStreamException {
		// sample code from darcs source code
		IModule module = parse("module Curl ( copyUrl, Cachable(Cachable, Uncachable, MaxAge) )\n" +
				"where\n" +
				"\n" +
				"  data Cachable = Cachable | Uncachable | MaxAge !CInt");
		
		IDeclaration dataDecl = module.getDeclarations()[0];
		assertTrue(dataDecl instanceof IDataDeclaration);
		
		ISourceLocation srcLoc = dataDecl.getSourceLocation();
		assertEquals(3, srcLoc.getLine());
		assertEquals(2, srcLoc.getColumn());
	}

	public void testNewtypeLocationRecording() throws RecognitionException, TokenStreamException {
		// sample code from darcs source code
		IModule module = parse("module Curl ( copyUrl, Cachable(Cachable, Uncachable, MaxAge) )\n" +
				"where\n" +
				" newtype Eq t => Renamed = Name [Char]\n");
		
		IDeclaration dataDecl = module.getDeclarations()[0];
		assertTrue(dataDecl instanceof INewTypeDeclaration);
		
		ISourceLocation srcLoc = dataDecl.getSourceLocation();
		assertEquals(2, srcLoc.getLine());
		assertEquals(1, srcLoc.getColumn());
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
		assertEquals(2, decls.length);
		assertEquals("VerySimple", decls[0].getName());
		assertEquals(1, decls[0].getSourceLocation().getLine());
		assertEquals(4, decls[0].getSourceLocation().getColumn());
		assertTrue(decls[0] instanceof IClassDeclaration);
		
		assertEquals("Eq", decls[1].getName());
		assertTrue(decls[1] instanceof IClassDeclaration);
		
		IClassDeclaration classDecl = (IClassDeclaration) decls[1];
		final ITypeSignature[] typeSigs = classDecl.getTypeSignatures();
		
		assertEquals(1, typeSigs.length);

		final String[] ids = typeSigs[0].getIdentifiers();
		assertEquals("==", ids[0]);
		assertEquals("/=", ids[1]);
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
	
	public void testDeclarationsAfterClass() throws RecognitionException, TokenStreamException {
		final String input = "class TestClass c where\n" +
				             "    test :: Int -> Int\n" +
				             "\n" +
				             "fat 0 = 1\n";
		IModule module = parse(input);
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		final ITypeSignature[] tsigs =
			((IClassDeclaration) decls[0]).getTypeSignatures();
		assertEquals("test", tsigs[0].getIdentifiers()[0]);
		
		assertEquals("fat", decls[1].getName());
	}
	
	public void testTwoClassDeclarations() throws RecognitionException, TokenStreamException {
		final String input = "class TestClassA a where\n" +
                             "    testA :: Int -> Int\n" +
                             "    testA2 :: String -> Int -> Int\n" +
                             "\n" +
                             "fat 0 = 1\n" +
                             "fat n = n * (fat (n - 1))\n" +
                             "\n" +
                             "class TestClassB b where\n" +
                             "    testB :: Char -> Int\n";
		IModule module = parse(input);
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(3, decls.length);
		
		IClassDeclaration classDeclA = (IClassDeclaration) decls[0];
		assertEquals("TestClassA", classDeclA.getName());
		assertEquals("testA", classDeclA.getTypeSignatures()[0].getIdentifiers()[0]);
		assertEquals("testA2", classDeclA.getTypeSignatures()[1].getIdentifiers()[0]);
		
		assertEquals("fat", decls[1].getName());

		IClassDeclaration classDeclB = (IClassDeclaration) decls[2];
		assertEquals("TestClassB", classDeclB.getName());
		assertEquals("testB", classDeclB.getTypeSignatures()[0].getIdentifiers()[0]);
	}
	
	public void testTypeSignatureAfterClassDeclaration() throws RecognitionException, TokenStreamException {
		final String input = "class TestClassA a where\n" +
 					         "    testA :: Int -> Int\n" +
					         "    testA2 :: String -> Int -> Int\n" +
					         "    testA3 x = x\n" +
					         "\n" +
					         "fat :: Int -> Int\n" +
					         "fat 0 = 1\n" +
					         "fat n = n * (fat (n - 1))\n";
		IModule module = parse(input);
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(3, decls.length);
		
		assertTrue(decls[1] instanceof ITypeSignature);
		assertEquals("fat", decls[1].getName());
	}
	
	public void testSimpleInstanceDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
				               "  instance FooInstance Foo \n" +
				               "  instance BarInstance Bar where {}\n");
		
		assertEquals("ParserTest", module.getName());

		IDeclaration[] decls = module.getDeclarations();
		assertEquals("FooInstance", decls[0].getName());
		assertTrue(decls[0] instanceof IInstanceDeclaration);
		
		assertEquals("BarInstance", decls[1].getName());
		assertTrue(decls[1] instanceof IInstanceDeclaration);
	}
	
//	TODO what should the instance declarations look like on the outline view?

	public void testContextInstanceDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
                               "  instance (Eq a, Show a) => Bar Foo where");
		
		assertEquals("ParserTest", module.getName());
		
		IDeclaration instDecl = module.getDeclarations()[0];
		assertEquals("Bar", instDecl.getName());
		assertTrue(instDecl instanceof IInstanceDeclaration);
	}
	
	public void testParenthysedInstanceDeclaration() throws RecognitionException, TokenStreamException {
		final IModule module = parse("module ParserTest where\n" +
				                      "instance Monad (SM s) where\n");
		
		IDeclaration instDecl = module.getDeclarations()[0];
		assertTrue(instDecl instanceof IInstanceDeclaration);
		assertEquals("Monad", instDecl.getName());
	}
	
	public void testListTypeInstanceDeclaration() throws RecognitionException, TokenStreamException {
		final IModule module = parse("module ParserTest where\n" +
        							 "instance Stringalike [PackedString] where\n");

		IDeclaration instDecl = module.getDeclarations()[0];
		assertTrue(instDecl instanceof IInstanceDeclaration);
		assertEquals("Stringalike", instDecl.getName());
	}
	
	public void testDefaultDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
				               "  default (Integer, Double)\n");
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals("default declaration", decls[0].getName());
		assertTrue(decls[0] instanceof IDefaultDeclaration);
	}
	
	public void testSimpleTypeSignature() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
        					   "  fat :: Int -> Int\n" +
        					   "  fat 0 = 1\n" +
        					   "  fat n = n * (fat (n - 1))");
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals("fat", decls[0].getName());
		assertTrue(decls[0] instanceof ITypeSignature);
	}
	
	public void testMultipleTypeSignature() throws RecognitionException, TokenStreamException {
		IModule module = parse("idf2, idf3 :: Int\n" + 
                               "idf2 = 42");
		
		final ITypeSignature ts = (ITypeSignature) module.getDeclarations()[0];
		final String[] ids = ts.getIdentifiers();
		
		assertEquals(2, ids.length);
		assertEquals("idf2", ids[0]);
		assertEquals("idf3", ids[1]);
	}
	
	public void testTypeSignatureWithContext() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserTest where\n" +
							   "  fat :: Int a => a -> a\n" +
							   "  fat 0 = 1\n" +
							   "  fat n = n * (fat (n - 1))");
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals("fat", decls[0].getName());
		assertTrue(decls[0] instanceof ITypeSignature);
	}
	
	public void testIgnoreNonStandardDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserImpl where {\n" +
				"foreign export stdcall \"parseHaskellCU\";\n" +
				"haskellParseCU :: CString -> IO ( StablePtr ( ParseResult HsModule ) );\n" +
				"haskellParseCU s = do {\n" +
				"                       cs <- ( peekCString s )\n" +
				"                       newStablePtr( parseModule cs )\n" +
				"                   }\n" +
				"}");
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertEquals(2, decls[0].getSourceLocation().getLine());
		assertEquals(3, decls[1].getSourceLocation().getLine());
	}

	public void testEndWithNonStandardDeclaration() throws RecognitionException, TokenStreamException {
		IModule module = parse("module ParserImpl where {\n" +
				"haskellParseCU :: CString -> IO ( StablePtr ( ParseResult HsModule ) );\n" +
				"haskellParseCU s = do {\n" +
				"                       cs <- ( peekCString s )\n" +
				"                       newStablePtr( parseModule cs )\n" +
				"                   };\n" +
				"foreign export stdcall \"parseHaskellCU\"\n" +
				"}");
		
		assertEquals(2, module.getDeclarations().length);
	}
	
	public void testContextAfterDatatypeDeclaration() throws RecognitionException, TokenStreamException {
		final String input = "data RepoPatchInfo = RPI String PatchInfo\n" +
				             "readPatchInfo :: Stringalike s => s -> Maybe (PatchInfo, s)";
		
		IModule module = parse(input);
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertEquals("RepoPatchInfo", decls[0].getName());
		assertEquals("readPatchInfo", decls[1].getName());
	}
	
	public void testFunctionDeclarationWithLabeledPattern() throws RecognitionException, TokenStreamException {
		//code shamelessly copied from darcs
		final String input = "command_alloptions DarcsCommand { command_darcsoptions = opts }\n" +
				             "    = opts ++ [disable, help, posthook_cmd, posthook_prompt]\n" +
				             "command_options = concat . map option_from_darcsoption . command_alloptions";
		
		IModule module = parse(input);
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertEquals("command_alloptions", decls[0].getName());
		assertEquals("command_options", decls[1].getName());
	}
	
	public void testInfixOperatorDeclaration() throws RecognitionException, TokenStreamException {
		final String input = "a <> b = not (a == b)";
		IModule module = parse(input);
		
		assertEquals("<>", module.getDeclarations()[0].getName());
	}
	
	public void testInfixOperatorTypeSignature() throws RecognitionException, TokenStreamException {
		final String input = "(<>) :: a -> a -> Bool";

		IModule module = parse(input);
		
		assertEquals("<>", module.getDeclarations()[0].getName());
	}
	
	public void testInfixFunctionDefinition() throws RecognitionException, TokenStreamException {
		//another code excerpt shamelessly copied from darcs
		final String input = "a `mand` b = do isa <- a" +
				             "    if isa then b else return False\n";
		IModule module = parse(input);
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals(1, decls.length);
		assertEquals("mand", decls[0].getName());
	}
	
	public void testVarsymsAtExportsSpec() throws RecognitionException, TokenStreamException {
		final String input = "module MathExtensions((&&), (**)) where\n";
		IModule module = parse(input);
		
		IExportSpecification[] exports = module.getExportSpecifications();
		assertEquals(2, exports.length);
		assertEquals("&&", exports[0].getName());
		assertEquals("**", exports[1].getName());
	}
	
	public void testPrefixVarsymAsFunctionId() throws RecognitionException, TokenStreamException {
		final String input = "(==) a b = not (a /= b)";
		IModule module = parse(input);
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(1, decls.length);
		assertEquals("==", decls[0].getName());
	}
	
	public void testSimpleFixityDeclaration() throws RecognitionException, TokenStreamException {
		final String input = "infix ++\n" +
				             "infixr 1 `op2`\n" +
				             "infixl 5 `op3`\n" +
				             "\n" +
				             "infix 5 `op0`, `op1`\n" +
				             "infixl 0 `op1`, +, `opx`\n" +
				             "infix 0o10 :+";
		IModule module = parse(input);
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(6, decls.length);
		
	    IInfixDeclaration decl = ( IInfixDeclaration )decls[ 0 ];
	    assertEquals( 9, decl.getPrecedenceLevel() );
	    assertEquals( IInfixDeclaration.ASSOC_NONE, decl.getAssociativity() );
	    assertEquals( 1, decl.getOperators().length );
	    assertEquals( "++", decl.getOperators()[ 0 ] );
	    
	    decl = ( IInfixDeclaration )decls[ 1 ];
	    assertEquals( 1, decl.getPrecedenceLevel() );
	    assertEquals( IInfixDeclaration.ASSOC_RIGHT, decl.getAssociativity() );
	    assertEquals( 1, decl.getOperators().length );
	    assertEquals( "op2", decl.getOperators()[ 0 ] );

	    decl = ( IInfixDeclaration )decls[ 4 ];
	    assertEquals( IInfixDeclaration.ASSOC_LEFT, decl.getAssociativity() );
	    assertEquals( 3, decl.getOperators().length );
	    assertEquals( "op1", decl.getOperators()[ 0 ] );
	    assertEquals( "+", decl.getOperators()[ 1 ] );
	    assertEquals( "opx", decl.getOperators()[ 2 ] );

	    decl = ( IInfixDeclaration )decls[ 5 ];
	    assertEquals( 8, decl.getPrecedenceLevel() );
	    assertEquals( ":+", decl.getOperators()[ 0 ] );
	}
	
	public void testSilentlyIgnoreGeneralisedDatatypes() throws RecognitionException, TokenStreamException {
		//sample code inspired on code from  Chris Kuklewicz
		final String input = "module AbsNum where\n" +
				             "\n" +
				             "data T :: * -> * where\n" +
				             "   -- Base\n" +
				             "   Var :: (Simplify a) => SymName -> T a\n" +
				             "   Const :: (Simplify a) => a -> T a\n" +
				             "\n" +
				             "abs a = if a < 0 then -a else a";
		
		IModule module = parse(input);
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals(1, decls.length);
		assertEquals("abs", decls[0].getName());
	}
	
//TODO recognize infix functions named with symbols (like <>, <+> and $$)
//see darcs source code, Printer.lhs
	
//TODO test the gtycon (that occurs inside inst, subrule of instancedecl)
	
//TODO decide what should be the name for the instance declarations below:
// instance Eq (a, b)
// instance Eq [a]
// instance Eq (a -> b)
	
//TODO take a look at the decl definition from Language.Haskell.Parser
	
	
	private static void assertEmpty(Object[] exports) {
		assertEquals(0, exports.length);
	}

	@Override
	protected void tearDown() throws Exception {
		System.setErr(fOldErrStream);
	}
	
	//TODO should be able to build a tree with a partial parse, not just
	//with valid inputs
	
	//TODO pay attention to the varsym rules (they can appear anywhere a
	//normal var appears)
	
	
	
}
