package net.sf.eclipsefp.haskell.ui.test.editor.codeassist;

import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

import net.sf.eclipsefp.haskell.ui.test.editor.codeassist.doubles.MockCompletionEngine;
import net.sf.eclipsefp.haskell.ui.test.editor.codeassist.doubles.StubViewer;

import de.leiffrenzel.fp.haskell.ui.editor.codeassist.HaskellCAProcessor;
import junit.framework.TestCase;

public class HaskellContentAssistProcessorTest extends TestCase {
	
	public void testCallsCompletionEngine() {
		final String input = "module CompletionEngineTest where\n" +
                             "\n" +
                             "putStr str = str\n" +
                             "\n" +
                             "main = pu\n";
		final int inputOffset = 62;

		MockCompletionEngine engine = new MockCompletionEngine();
		engine.setExpectedOffset(inputOffset);
		engine.setOutput(new String[] { "putStr" });

		HaskellCAProcessor processor = new HaskellCAProcessor(engine);
		ICompletionProposal[] props = processor.computeCompletionProposals(
				                          new StubViewer(input), inputOffset);
		
		assertNotNull(props);
		assertContains(new CompletionProposal("putStr", inputOffset - 2, 6, 68), props);
		
		engine.verify();
	}

	private void assertContains(ICompletionProposal expected, ICompletionProposal[] props) {
		String expectedContent = applyToEmptyDocument(expected);

		for(ICompletionProposal prop: props) {
			String actualContent = applyToEmptyDocument(prop);;
			if (expectedContent.equals(actualContent)) {
				return;
			}
		}
		
		fail("Proposal not found");
	}

	private String applyToEmptyDocument(ICompletionProposal expected) {
		IDocument expectedDoc = createDocument(expected.getSelection(null).x);
		expected.apply(expectedDoc);
		String expectedContent = expectedDoc.get();
		return expectedContent;
	}

	private IDocument createDocument(int size) {
		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < size; ++i)
			buf.append(' ');
		return new Document(buf.toString());
	}

}
