package net.sf.eclipsefp.haskell.core.test.halamo;


import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.halamo.Halamo;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.Scope;

import net.sf.eclipsefp.haskell.core.test.util.HalamoAssert;
import net.sf.eclipsefp.test.util.common.MockFile;
import net.sf.eclipsefp.test.util.haskell.HaskellProject_PDETestCase;

public class LanguageModel_PDETestCase extends HaskellProject_PDETestCase {
	
	private Halamo fLangModelEngine = Halamo.getInstance();;

	@Override
	protected void setUpMore() throws Exception {
		final String fibContents = "module Fibonacci where\n" +
                                   "\n" +
                                   "fib 1 = 0\n" +
                                   "fib 2 = 1\n" +
                                   "fib n = (fibb (n - 1)) + (fib (n - 2))";
		final String facContents = "module Factorial where\n" +
                                   "\n" +
                                   "fac 0 = 1\n" +
                                   "fac n = n * (fac (n - 1))\n";
		createSourceFile(fibContents, "Fibonacci.hs");
		createSourceFile(facContents, "Factorial.hs");
	}

	public void testExtendedScopeWithImport() throws CoreException {
		final String mainContents = "module Main where\n" +
        							"\n" +
        							"import Fibonacci\n" +
        							"\n" +
        							"main = putStr $ show $ fib 5";
		IFile mainFile = createSourceFile(mainContents, "Main.hs");
		
		Scope scope = fLangModelEngine.getScopeFor(mainFile);
		assertNotNull(scope);
		
		List<IModule> modules = scope.getAvailableModules();
		IModule fibModule = HalamoAssert.assertContains("Fibonacci", modules);
		
		assertEquals("fib", fibModule.getDeclarations()[0].getName());
	}
	
	public void testUnitWithNoImports() throws CoreException {
		final String contents = "module WrongFactorial where\n" +
				                      "\n" +
				                      "fat 0 = 1";
		IFile file = createSourceFile(contents, "WrongFactorial.hs");
		
		Scope scope = fLangModelEngine.getScopeFor(file);
		assertNotNull(scope);
	}
	
	public void testWithMoreThanOneImport() throws CoreException {
		final String contents = "module Main where\n" +
									"\n" +
									"import Fibonacci\n" +
									"import Factorial\n" +
									"\n" +
									"main = putStr $ show $ fib $ fac 3";
		
		IFile file = createSourceFile(contents, "Main.hs");
		
		Scope scope = fLangModelEngine.getScopeFor(file);
		
		assertNotNull(scope);
		
		List<IModule> modules = scope.getAvailableModules();

		HalamoAssert.assertContains("Fibonacci", modules);
		HalamoAssert.assertContains("Factorial", modules);
		
		List<IDeclaration> declarations = scope.getAvailableDeclarations();
		
		HalamoAssert.assertContains("fib", declarations);
		HalamoAssert.assertContains("fac", declarations);
	}
	
	public void testFailGracefullyWithFileOusideProject() {
		final String contents = "module Main where\n" +
				                "\n" +
				                "main = putStr \"Hello, world!\\n\"";
		MockFile file = new MockFile(contents);
		
		file.setProject(null);
		
		Scope scope = fLangModelEngine.getScopeFor(file);
		assertNotNull(scope);
	}

}
