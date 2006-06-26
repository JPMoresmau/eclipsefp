package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import static org.easymock.EasyMock.createNiceMock;

import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IExportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.jparser.ast.Module;
import junit.framework.TestCase;

public class ModuleTest extends TestCase {
	
	public void testNewEmptyModule() {
		IModule module = new Module();
		
		assertTrue(module.isEmpty());
	}
	
	public void testModuleWithImport() {
		Module module = new Module();
		
		module.addImport(createNiceMock(IImport.class));
		
		assertFalse(module.isEmpty());
	}
	
	public void testModuleWithDeclaration() {
		Module module = new Module();

		module.addDeclaration(createNiceMock(IDeclaration.class));
		
		assertFalse(module.isEmpty());
	}

	public void testModuleWithExport() {
		Module module = new Module();

		module.addExport(createNiceMock(IExportSpecification.class));
		
		assertFalse(module.isEmpty());
	}
	
	public void testModuleWithName() {
		Module module = new Module();
		
		module.setName("Unimportant");
		
		assertFalse(module.isEmpty());
	}

}
