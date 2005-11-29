package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import junit.framework.TestCase;

public class FunctionBindingTest extends TestCase {
	
	public void testInitialization() {
		IFunctionBinding binding = new FunctionBinding();
		
		assertNotNull(binding.getMatches());
		assertEquals(0, binding.getMatches().length);
	}

}
