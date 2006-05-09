package net.sf.eclipsefp.haskell.core.test.compiler;

import net.sf.eclipsefp.haskell.core.compiler.CompilerOutputItem;
import junit.framework.TestCase;

public class CompilerOutputItemTest extends TestCase {
	
	public void testAddComment() {
		CompilerOutputItem createdItem = new CompilerOutputItem("test.hs", 1, "0:");
		createdItem.addToComment("Comment line 1");
		createdItem.addToComment("Comment line 2");
		
		final String expectedComment = "0:\n" +
	                                   "Comment line 1\n" +
	                                   "Comment line 2";
		assertEquals(expectedComment, createdItem.getComment());
	}
	
}
