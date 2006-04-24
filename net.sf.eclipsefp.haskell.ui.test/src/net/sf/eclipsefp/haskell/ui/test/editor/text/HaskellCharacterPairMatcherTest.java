package net.sf.eclipsefp.haskell.ui.test.editor.text;

import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.source.ICharacterPairMatcher;

import de.leiffrenzel.fp.haskell.ui.editor.text.HaskellCharacterPairMatcher;

import junit.framework.TestCase;

public class HaskellCharacterPairMatcherTest extends TestCase {
	
	private ICharacterPairMatcher fMatcher;

	@Override
	protected void setUp() throws Exception {
		fMatcher = new HaskellCharacterPairMatcher();
	}

	public void testMatchesGivenClosingParentheses() {
		assertMatches(6, 6, "qsort (a:as) = ", 12);
	}
	
	public void testMatchesGivenOpeningParentheses() {
		assertMatches(6, 6, "qsort (a:as) = ", 7);
	}

	public void testMatchesNestedParentheses() {
		assertMatches(12, 12, "fat n = n * (fat (n - 1))", 13);
		assertMatches(12, 12, "fat n = n * (fat (n - 1))", 25);
	}

	private void assertMatches(int expectedOffset, int expectedLength, String contents, int start) {
		IDocument doc = createDocument(contents);
		IRegion actual = fMatcher.match(doc, start);
		
		assertNotNull(actual);
		assertTrue("expected [" + expectedOffset + ", " + expectedLength +
				   "] but was [" + actual.getOffset() + ", " +
				   actual.getLength() + "]",
				      expectedOffset == actual.getOffset()
				   && expectedLength == actual.getLength());
	}

	private IDocument createDocument(final String contents) {
		return new Document(contents);
	}
	
}
