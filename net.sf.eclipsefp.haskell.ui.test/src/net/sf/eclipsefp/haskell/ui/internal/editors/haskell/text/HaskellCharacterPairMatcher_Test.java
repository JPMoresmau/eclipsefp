package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import junit.framework.TestCase;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.source.ICharacterPairMatcher;

public class HaskellCharacterPairMatcher_Test extends TestCase {

	private ICharacterPairMatcher fMatcher;


	// interface methods of TestCase
	////////////////////////////////

	@Override
	protected void setUp() throws Exception {
		fMatcher = new HaskellCharacterPairMatcher();
	}


	// test cases
	/////////////

	public void testMatchesGivenClosingParentheses() {
		assertMatches(6, 6, "qsort (a:as) = ", 12);
		assertAnchor(ICharacterPairMatcher.RIGHT);
	}

	public void testMatchesGivenOpeningParentheses() {
		assertMatches(6, 6, "qsort (a:as) = ", 7);
		assertAnchor(ICharacterPairMatcher.LEFT);
	}

	public void testMatchesGivenOpeningBrackets() {
		assertMatches(23, 3, "qsort (a:as) = left ++ [x] ++ right", 24);
		assertAnchor(ICharacterPairMatcher.LEFT);
	}

	public void testMatchesGivenClosingBraces() {
		assertMatches(23, 5, "module QuickSort where {  \n}", 28);
		assertAnchor(ICharacterPairMatcher.RIGHT);
	}

	public void testMatchesNestedParentheses() {
		assertMatches(12, 13, "fat n = n * (fat (n - 1))", 13);
		assertMatches(12, 13, "fat n = n * (fat (n - 1))", 25);
		assertMatches(17, 7, "fat n = n * (fat (n - 1))", 18);
		assertMatches(17, 7, "fat n = n * (fat (n - 1))", 24);
	}

	public void testMatchesMixedPairs() {
		assertMatches(0, 16, "[(12), (13 + 1)]", 1);
		assertMatches(1, 4, "[(12), (13 + 1)]", 2);
		assertMatches(7, 8, "[(12), (13 + 1)]", 15);
	}

	public void testAcceptsOffsetZero() {
		IDocument doc = createDocument("");
		assertNull(fMatcher.match(doc, 0));
	}

	public void testDoNotMatchArbitrayChar() {
		IDocument doc = createDocument("qsort (a:as) = ");
		assertNull(fMatcher.match(doc, 4));
	}


	// helping functions
	////////////////////

	private void assertMatches(final int expectedOffset, final int expectedLength, final String contents, final int start) {
		IDocument doc = createDocument(contents);
		IRegion actual = fMatcher.match(doc, start);

		assertNotNull(actual);
		assertTrue("expected [" + expectedOffset + ", " + expectedLength +
				   "] but was [" + actual.getOffset() + ", " +
				   actual.getLength() + "]",
				      expectedOffset == actual.getOffset()
				   && expectedLength == actual.getLength());
	}

	private void assertAnchor(final int expectedAnchor) {
		assertEquals(expectedAnchor, fMatcher.getAnchor());
	}

	private IDocument createDocument(final String contents) {
		return new Document(contents);
	}

}
