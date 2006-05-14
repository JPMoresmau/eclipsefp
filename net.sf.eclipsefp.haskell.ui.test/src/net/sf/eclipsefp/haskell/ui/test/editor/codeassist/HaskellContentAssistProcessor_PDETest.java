package net.sf.eclipsefp.haskell.ui.test.editor.codeassist;

import org.eclipse.jface.text.contentassist.ICompletionProposal;

import net.sf.eclipsefp.haskell.core.test.util.CompletionProposalTestCase;
import net.sf.eclipsefp.haskell.ui.editor.codeassist.HaskellCAProcessor;
import net.sf.eclipsefp.haskell.ui.test.editor.codeassist.doubles.MockCompletionEngine;
import net.sf.eclipsefp.haskell.ui.test.editor.codeassist.doubles.StubViewer;

public class HaskellContentAssistProcessor_PDETest extends CompletionProposalTestCase {
	
	public void testCallsCompletionEngine() {
		final String input = "module CompletionEngineTest where\n" +
                             "\n" +
                             "putStr str = str\n" +
                             "\n" +
                             "main = pu";
		final int inputOffset = 62;

		final ICompletionProposal proposalResult = createProposal("pu", "putStr", 62);
		
		MockCompletionEngine engine = new MockCompletionEngine();
		engine.setExpectedOffset(inputOffset);
		engine.setOutput(new ICompletionProposal[] {proposalResult});

		HaskellCAProcessor processor = new HaskellCAProcessor(engine);
		ICompletionProposal[] props = processor.computeCompletionProposals(
				                          new StubViewer(input), inputOffset);
		
		assertNotNull(props);
		assertContains(proposalResult, props);
		
		engine.verify();
	}

}
