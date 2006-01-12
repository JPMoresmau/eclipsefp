package net.sf.eclipsefp.haskell.core.jparser.ast.test;

import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;
import net.sf.eclipsefp.haskell.core.jparser.ast.SourceLocation;
import junit.framework.TestCase;

public class SourceLocationTest extends TestCase {
	
	private ISourceLocation aOneTwoLocation         = new SourceLocation(1, 2);
	private ISourceLocation aOneThreeLocation       = new SourceLocation(1, 3);
	private ISourceLocation aTwoTwoLocation         = new SourceLocation(2, 2);
	private ISourceLocation anotherOneTwoLocation   = new SourceLocation(1, 2);
	private ISourceLocation anotherOneThreeLocation = new SourceLocation(1, 3);
	
	public void testIsBefore() {
		assertTrue(aOneTwoLocation.isBefore(aOneThreeLocation));
		assertFalse(aOneThreeLocation.isBefore(aOneTwoLocation));
		assertFalse(aOneTwoLocation.isBefore(anotherOneTwoLocation));
		assertTrue(aOneTwoLocation.isBefore(aTwoTwoLocation));
		assertFalse(aTwoTwoLocation.isBefore(aOneTwoLocation));
	}
	
	public void testIsAfter() {
		assertTrue(aOneThreeLocation.isAfter(aOneTwoLocation));
		assertFalse(aOneTwoLocation.isAfter(aOneThreeLocation));
		assertFalse(aOneThreeLocation.isAfter(anotherOneThreeLocation));
		assertFalse(aOneThreeLocation.isAfter(aTwoTwoLocation));
		assertTrue(aTwoTwoLocation.isAfter(aOneThreeLocation));
	}
	
}
