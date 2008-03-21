package net.sf.eclipsefp.haskell.core.halamo;

import java.util.Collection;
import java.util.List;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.internal.doubles.StubHalamo;
import net.sf.eclipsefp.haskell.core.internal.doubles.StubModule;
import net.sf.eclipsefp.haskell.core.internal.util.HalamoAssert;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionBinding;
import net.sf.eclipsefp.haskell.core.jparser.ast.Module;
import net.sf.eclipsefp.haskell.core.jparser.ast.TypeSignature;

public class Scope_Test extends TestCase {

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

	public void testFilterTypeSignatures() {
		Scope scope = new Scope();

		Module modFib = new Module();
		Module modFac = new Module();

		modFib.setName("Fibonacci");
		modFac.setName("Factorial");

		TypeSignature fibTs = new TypeSignature();
		FunctionBinding fibFb = new FunctionBinding();
		fibTs.setName("fibb");
		fibFb.setName("fibb");
		modFib.addDeclaration(fibTs);
		modFib.addDeclaration(fibFb);

		FunctionBinding facFb = new FunctionBinding();
		facFb.setName("fac");
		modFac.addDeclaration(facFb);

		scope.addAvailableModule(modFib);
		scope.addAvailableModule(modFac);

		List<IDeclaration> decls = scope.getCreatingDeclarations();
		assertEquals(2, decls.size());
		assertContains("fibb", decls);
	}

	private void assertContains(final String name, final List<IDeclaration> decls) {
		for(IDeclaration decl : decls) {
			if (name.equals(decl.getName())) {
				return;
			}
		}
		fail("Declaration " + name + " not found");
	}

}
