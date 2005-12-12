package net.sf.eclipsefp.haskell.core.jparser.test;

import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.jparser.ModuleBuilder;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionMatch;
import junit.framework.TestCase;

public class ModuleBuilderTest extends TestCase {
	
	private ModuleBuilder fBuilder = new ModuleBuilder();

	public void testBuildDifferentModules() {
		fBuilder.startModule();
		
		Object fstModule = fBuilder.getResult();
		
		fBuilder.startModule();

		Object sndModule = fBuilder.getResult();
		
		assertNotSame(fstModule, sndModule);
	}
	
	public void testGroupMatchesInBindings() {
		fBuilder.startModule();
		
		fBuilder.addFunctionMatch(createFunctionMatch("fat"));
		fBuilder.addFunctionMatch(createFunctionMatch("fat"));
		
		fBuilder.addFunctionMatch(createFunctionMatch("fib"));
		fBuilder.addFunctionMatch(createFunctionMatch("fib"));
		fBuilder.addFunctionMatch(createFunctionMatch("fib"));

		IModule module = fBuilder.getResult();
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertEquals("fat", decls[0].getName());
		assertEquals(2, ((IFunctionBinding) decls[0]).getMatches().length);
		assertEquals("fib", decls[1].getName());
		assertEquals(3, ((IFunctionBinding) decls[1]).getMatches().length);
		
	}

	private IMatch createFunctionMatch(String name) {
		FunctionMatch fstFatMatch = new FunctionMatch();
		fstFatMatch.setName(name);
		return fstFatMatch;
	}
	
}