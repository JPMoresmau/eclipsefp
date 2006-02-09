package net.sf.eclipsefp.haskell.core.test.util;

import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

import junit.framework.TestCase;

public class CompletionProposalTestCase extends TestCase {
	
	public static ICompletionProposal createProposal(String replaced, String replacement, int offset) {
		final int qlen = replaced.length();
		return new CompletionProposal(replacement, offset - qlen, qlen,
				                      replacement.length());
	}

	
	public static void assertContains(ICompletionProposal expected, ICompletionProposal[] props) {
		String expectedContent = applyToEmptyDocument(expected);

		for(ICompletionProposal prop: props) {
			String actualContent = applyToEmptyDocument(prop);;
			if (expectedContent.equals(actualContent)) {
				return;
			}
		}
		
		fail("Proposal not found");
	}

	private static String applyToEmptyDocument(ICompletionProposal expected) {
		IDocument expectedDoc = createDocument(expected.getSelection(null).x);
		expected.apply(expectedDoc);
		String expectedContent = expectedDoc.get();
		return expectedContent;
	}

	private static IDocument createDocument(int size) {
		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < size; ++i)
			buf.append(' ');
		return new Document(buf.toString());
	}

}
