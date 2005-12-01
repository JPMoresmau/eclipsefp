package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import net.sf.eclipsefp.haskell.core.jparser.ast.ClassDeclaration;
import junit.framework.TestCase;

public class ClassDeclarationTest extends TestCase {
	
	public void testInitialization() {
		ClassDeclaration decl = new ClassDeclaration();
		
		assertNotNull(decl.getTypeSignatures());
	}

}
