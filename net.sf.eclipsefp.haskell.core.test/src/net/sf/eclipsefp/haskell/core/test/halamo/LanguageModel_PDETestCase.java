package net.sf.eclipsefp.haskell.core.test.halamo;


import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.halamo.Halamo;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.Scope;

import net.sf.eclipsefp.haskell.core.test.util.HalamoAssert;
import net.sf.eclipsefp.test.util.haskell.HaskellProject_PDETestCase;

public class LanguageModel_PDETestCase extends HaskellProject_PDETestCase {
	
	public void testExtendedScopeWithImport() throws CoreException {
		final String fibContents = "module Fibonacci where\n" +
				                   "\n" +
				                   "fib 1 = 0\n" +
				                   "fib 2 = 1\n" +
				                   "fib n = (fibb (n - 1)) + (fib (n - 2))";
		final String mainContents = "module Main where\n" +
        							"\n" +
        							"import Fibonacci\n" +
        							"\n" +
        							"main = putStr $ show $ fib 5";

		createSourceFile(fibContents, "Fibonacci.hs");
		IFile mainFile = createSourceFile(mainContents, "Main.hs");
		
		Halamo langModelEngine = Halamo.getInstance();
		
		Scope scope = langModelEngine.getScopeFor(mainFile);
		assertNotNull(scope);
		
		List<IModule> modules = scope.getAvailableModules();
		IModule fibModule = HalamoAssert.assertContains("Fibonacci", modules);
		
		assertEquals("fib", fibModule.getDeclarations()[0].getName());
	}

}
