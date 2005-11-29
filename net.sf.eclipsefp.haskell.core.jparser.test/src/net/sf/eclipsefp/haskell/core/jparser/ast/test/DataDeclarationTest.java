package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import net.sf.eclipsefp.haskell.core.jparser.ast.DataDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IDataDeclaration;
import junit.framework.TestCase;

public class DataDeclarationTest extends TestCase {
	
	public void testInitialization() {
		
		IDataDeclaration decl = new DataDeclaration();
		
		assertNotNull(decl.getConstructors());
		
	}

}
