package net.sf.eclipsefp.haskell.core.test.halamo;

import java.util.List;

import net.sf.eclipsefp.haskell.core.test.internal.doubles.StubModule;
import net.sf.eclipsefp.haskell.core.test.util.HalamoAssert;

import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.Scope;
import junit.framework.TestCase;

public class ScopeTest extends TestCase {
	
	public void testFlattenDeclarations() {
		Scope scope = new Scope();
		
		scope.addAvailableModule(new StubModule("fib"));
		scope.addAvailableModule(new StubModule("fat"));
		
		List<IDeclaration> decls = scope.getAvailableDeclarations();
		HalamoAssert.assertContains("fib", decls);
	}

}
