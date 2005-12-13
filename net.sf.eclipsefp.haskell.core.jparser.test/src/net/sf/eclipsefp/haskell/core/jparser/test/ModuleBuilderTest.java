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
	
	public void testReturnsStartedModule() {
		final String moduleName = "ModuleBuilderTest";
		IModule module = fBuilder.startModule(moduleName);
		
		assertEquals(moduleName, module.getName());
	}
	
	public void testGroupMatchesInBindings() {
		fBuilder.startModule();
		
		fBuilder.addFunctionMatch(createFunctionMatch("fat", 1, 4));
		fBuilder.addFunctionMatch(createFunctionMatch("fat", 2, 4));
		
		fBuilder.addFunctionMatch(createFunctionMatch("fib", 4, 4));
		fBuilder.addFunctionMatch(createFunctionMatch("fib", 5, 4));
		fBuilder.addFunctionMatch(createFunctionMatch("fib", 6, 4));

		IModule module = fBuilder.getResult();
		
		IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertEquals("fat", decls[0].getName());
		assertEquals(2, ((IFunctionBinding) decls[0]).getMatches().length);
		assertEquals(1, decls[0].getSourceLocation().getLine());
		assertEquals(4, decls[0].getSourceLocation().getColumn());

		assertEquals("fib", decls[1].getName());
		assertEquals(3, ((IFunctionBinding) decls[1]).getMatches().length);
		assertEquals(4, decls[1].getSourceLocation().getLine());
		assertEquals(4, decls[1].getSourceLocation().getColumn());
		
	}

	private FunctionMatch createFunctionMatch(String name, int line, int column) {
		FunctionMatch match = new FunctionMatch();
		match.setName(name);
		match.setLocation(line, column);
		return match;
	}
	
}