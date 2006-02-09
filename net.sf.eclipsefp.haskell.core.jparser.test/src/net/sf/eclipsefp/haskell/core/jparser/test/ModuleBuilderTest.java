package net.sf.eclipsefp.haskell.core.jparser.test;

import de.leiffrenzel.fp.haskell.core.halamo.IClassDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IConstructor;
import de.leiffrenzel.fp.haskell.core.halamo.IDataDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeSignature;
import net.sf.eclipsefp.haskell.core.jparser.ModuleBuilder;
import net.sf.eclipsefp.haskell.core.jparser.ast.Constructor;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionMatch;
import net.sf.eclipsefp.haskell.core.jparser.ast.TypeSignature;
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

	public void testGroupClassDeclarationComponents() {
		final ITypeSignature tsig = createTypeSignature();

		fBuilder.startModule();
		fBuilder.startClassDeclaration();
		fBuilder.addTypeSignature(tsig);
		fBuilder.addFunctionMatch(createFunctionMatch("fat", 3, 4));
		
		IModule module = fBuilder.getResult();
		IDeclaration decl = module.getDeclarations()[0];
		assertTrue(decl instanceof IClassDeclaration);
		
		ITypeSignature[] tsigs = ((IClassDeclaration) decl).getTypeSignatures();
		assertEquals(1, tsigs.length);
		assertSame(tsig, tsigs[0]);
	}
	
	public void testEndClassDeclaration() {
		final ITypeSignature internalTSig = createTypeSignature();
		final ITypeSignature externalTSig = createTypeSignature();
		
		fBuilder.startModule();
		fBuilder.startClassDeclaration();
		fBuilder.addTypeSignature(internalTSig);
		fBuilder.endClassDeclaration();
		fBuilder.addTypeSignature(externalTSig);
		
		final IModule module = fBuilder.getResult();
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertTrue(decls[0] instanceof IClassDeclaration);
		
		IClassDeclaration classDecl = (IClassDeclaration) decls[0];
		assertSame(internalTSig, classDecl.getTypeSignatures()[0]);
		
		assertSame(externalTSig, decls[1]);
	}
	
	public void testGroupDataDeclarationConstructors() {
		final IConstructor cons = createConstructor();
		
		fBuilder.startModule();
		fBuilder.startDataDeclaration();
		fBuilder.addConstructor(cons);
		fBuilder.addFunctionMatch(createFunctionMatch("fat", 3, 4));
		
		final IModule module = fBuilder.getResult();
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertTrue(decls[0] instanceof IDataDeclaration);
		
		IDataDeclaration dataDecl = (IDataDeclaration) decls[0];
		assertSame(cons, dataDecl.getConstructors()[0]);
	}

	public void testDistributeConstructorsBetweenDataDeclarations() {
		final IConstructor fstCons = createConstructor();
		final IConstructor sndCons = createConstructor();
		
		fBuilder.startModule();
		fBuilder.startDataDeclaration();
		fBuilder.addConstructor(fstCons);
		
		fBuilder.startDataDeclaration();
		fBuilder.addConstructor(sndCons);
		
		final IModule module = fBuilder.getResult();
		
		final IDeclaration[] decls = module.getDeclarations();
		assertEquals(2, decls.length);
		assertTrue(decls[0] instanceof IDataDeclaration);
		assertTrue(decls[1] instanceof IDataDeclaration);
		
		IDataDeclaration fstDataDecl = (IDataDeclaration) decls[0];
		IConstructor[] fstConstructorGroup = fstDataDecl.getConstructors();
		assertEquals(1, fstConstructorGroup.length);
		assertSame(fstCons, fstConstructorGroup[0]);

		IDataDeclaration sndDataDecl = (IDataDeclaration) decls[1];
		IConstructor[] sndConstructorGroup = sndDataDecl.getConstructors();
		assertEquals(1, sndConstructorGroup.length);
		assertSame(sndCons, sndConstructorGroup[0]);
	}

	private IConstructor createConstructor() {
		return new Constructor();
	}

	private TypeSignature createTypeSignature() {
		TypeSignature typeSignature = new TypeSignature();
		return typeSignature;
	}
	
	private FunctionMatch createFunctionMatch(String name, int line, int column) {
		FunctionMatch match = new FunctionMatch();
		match.setName(name);
		match.setLocation(line, column, 0);
		return match;
	}
	
}