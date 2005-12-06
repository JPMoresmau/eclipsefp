package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import net.sf.eclipsefp.haskell.core.jparser.ast.TypeSignature;
import junit.framework.TestCase;

public class TypeSignatureTest extends TestCase {

	public void testInitialization() {
		TypeSignature tsig = new TypeSignature();
		
		assertNotNull(tsig.getIdentifiers());
	}

}
