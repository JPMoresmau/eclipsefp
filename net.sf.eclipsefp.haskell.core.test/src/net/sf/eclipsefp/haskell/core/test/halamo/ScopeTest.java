package net.sf.eclipsefp.haskell.core.test.halamo;

import java.util.Collection;
import java.util.List;

import net.sf.eclipsefp.haskell.core.test.internal.doubles.StubHalamo;
import net.sf.eclipsefp.haskell.core.test.internal.doubles.StubModule;
import net.sf.eclipsefp.haskell.core.test.internal.util.HalamoAssert;

import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.Scope;
import junit.framework.TestCase;

public class ScopeTest extends TestCase {
	
	public void testFlattenDeclarationLists() {
		Scope scope = new Scope();
		
		scope.addAvailableModule(new StubModule("Fibonacci", "fib"));
		scope.addAvailableModule(new StubModule("Factorial", "fat"));
		
		List<IDeclaration> decls = scope.getAvailableDeclarations();
		HalamoAssert.assertContains("fib", decls);
	}
	
	public void testImportableModules() {
		StubHalamo langModel = new StubHalamo();
		langModel.putModule(new StubModule("Fibonacci"));
		langModel.putModule(new StubModule("Factorial"));
		
		Scope scope = new Scope(new StubModule(), langModel);
		Collection<IModule> modules = scope.getImportableModules();
		HalamoAssert.assertContains("Fibonacci", modules);
		HalamoAssert.assertContains("Factorial", modules);
	}
	
	public void testFilterModulesNotInModel() {
		StubHalamo langModel = new StubHalamo();

		final StubModule module = new StubModule();
		module.addImport("QuickSort");
		Scope scope = new Scope(module, langModel);
		
		assertEquals(0, scope.getAvailableModules().size());
	}

}
