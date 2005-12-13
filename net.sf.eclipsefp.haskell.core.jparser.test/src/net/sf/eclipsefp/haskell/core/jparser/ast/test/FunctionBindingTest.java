package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionBinding;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionMatch;
import junit.framework.TestCase;

public class FunctionBindingTest extends TestCase {
	
	private FunctionBinding fBinding;

	public void testInitialization() {
		assertNotNull(fBinding.getMatches());
		assertEquals(0, fBinding.getMatches().length);
	}

	protected void setUp() {
		fBinding = new FunctionBinding();
	}
	
	public void testAcceptsMatch() {
		fBinding.setName("fat");
		FunctionMatch match = createMatch("fat");
		
		assertTrue("Did not accept valid match", fBinding.acceptsMatch(match));
	}

	public void testDoesNotAcceptMatch() {
		fBinding.setName("fat");
		FunctionMatch match = createMatch("fib");
		
		assertFalse("Did accept invalid match", fBinding.acceptsMatch(match));
	}
	
	public void testRejectInvalidMatch() {
		fBinding.setName("fat");
		FunctionMatch match = createMatch("fib");

		try {
			fBinding.addMatch(match);
			fail("Added invalid match");
		} catch(IllegalArgumentException e) {
			assertTrue(true);
		}
		
		assertEquals(0, fBinding.getMatches().length);
	}
	
	private FunctionMatch createMatch(String name) {
		FunctionMatch match = new FunctionMatch();
		match.setName(name);
		return match;
	}
	
}
