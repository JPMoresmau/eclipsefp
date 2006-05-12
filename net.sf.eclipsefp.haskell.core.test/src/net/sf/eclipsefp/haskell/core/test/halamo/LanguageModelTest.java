package net.sf.eclipsefp.haskell.core.test.halamo;


import java.io.StringReader;
import java.util.List;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

import org.eclipse.core.runtime.CoreException;

import static org.easymock.EasyMock.*;

import net.sf.eclipsefp.haskell.core.halamo.Halamo;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.Scope;
import net.sf.eclipsefp.haskell.core.jparser.HaskellParser;

import net.sf.eclipsefp.haskell.core.test.util.HalamoAssert;

public class LanguageModelTest extends TestCase {
	
	private Halamo fLangModelEngine = Halamo.getInstance();

	@Override
	protected void setUp() throws Exception {
		final String fibContents = "module Fibonacci where\n" +
                                   "\n" +
                                   "fib 1 = 0\n" +
                                   "fib 2 = 1\n" +
                                   "fib n = (fibb (n - 1)) + (fib (n - 2))";
		final String facContents = "module Factorial where\n" +
                                   "\n" +
                                   "fac 0 = 1\n" +
                                   "fac n = n * (fac (n - 1))\n";
		createModule(fibContents);
		createModule(facContents);
	}

	public void testExtendedScopeWithImport() throws CoreException {
		final String mainContents = "module Main where\n" +
        							"\n" +
        							"import Fibonacci\n" +
        							"\n" +
        							"main = putStr $ show $ fib 5";
		IModule mainModule = createModule(mainContents);
		
		Scope scope = fLangModelEngine.getScopeFor(mainModule);
		assertNotNull(scope);
		
		List<IModule> modules = scope.getAvailableModules();
		IModule fibModule = HalamoAssert.assertContains("Fibonacci", modules);
		
		assertEquals("fib", fibModule.getDeclarations()[0].getName());
	}
	
	public void testUnitWithNoImports() throws CoreException {
		final String contents = "module WrongFactorial where\n" +
				                      "\n" +
				                      "fat 0 = 1";
		IModule module = createModule(contents);
		
		Scope scope = fLangModelEngine.getScopeFor(module);
		assertNotNull(scope);
	}
	
	public void testWithMoreThanOneImport() throws CoreException {
		final String contents = "module Main where\n" +
									"\n" +
									"import Fibonacci\n" +
									"import Factorial\n" +
									"\n" +
									"main = putStr $ show $ fib $ fac 3";
		
		IModule module = createModule(contents);
		
		Scope scope = fLangModelEngine.getScopeFor(module);
		
		assertNotNull(scope);
		
		List<IModule> modules = scope.getAvailableModules();

		HalamoAssert.assertContains("Fibonacci", modules);
		HalamoAssert.assertContains("Factorial", modules);
		
		List<IDeclaration> declarations = scope.getAvailableDeclarations();
		
		HalamoAssert.assertContains("fib", declarations);
		HalamoAssert.assertContains("fac", declarations);
	}
	
	public void testFailGracefullyWithFileOusideProject() {
//TODO check this edge case
//		final String contents = "module Main where\n" +
//				                "\n" +
//				                "main = putStr \"Hello, world!\\n\"";
//		MockFile file = new MockFile(contents);
//		
//		file.setProject(null);
//		
//		Scope scope = fLangModelEngine.getScopeFor(file);
//		assertNotNull(scope);
	}
	
	public void testAddingModules() {
		IModule myModule = createModuleByName("MyModule");
		IModule otherModule = createModuleByName("OtherModule");
		fLangModelEngine.putModule(myModule);
		fLangModelEngine.putModule(otherModule);
		
		assertSame(otherModule, fLangModelEngine.getModule("OtherModule"));
		assertSame(myModule, fLangModelEngine.getModule("MyModule"));
	}
	
	private IModule createModuleByName(String name) {
		IModule module = createMock(IModule.class);
		expect(module.getName()).
			andReturn(name).
			anyTimes();
		replay(module);
		return module;
	}

	private IModule createModule(final String contents) {
		try {
			IModule module = new HaskellParser(new StringReader(contents)).parseModule();
			fLangModelEngine.putModule(module);
			return module;
		} catch (Exception e) {
			throw new AssertionFailedError(e.getMessage());
		}
	}

}
