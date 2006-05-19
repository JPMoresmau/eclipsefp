package net.sf.eclipsefp.haskell.ui.test.editor.codeassist;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;

import static org.easymock.EasyMock.anyInt; 
import static org.easymock.EasyMock.anyObject; 
import static org.easymock.EasyMock.expect; 
import static org.easymock.EasyMock.createMock; 
import static org.easymock.EasyMock.replay; 
import static org.easymock.EasyMock.verify; 

import net.sf.eclipsefp.haskell.core.codeassist.HaskellCompletionContext;
import net.sf.eclipsefp.haskell.core.codeassist.ICompletionEngine;
import net.sf.eclipsefp.haskell.core.test.util.CompletionProposalTestCase;
import net.sf.eclipsefp.haskell.ui.editor.codeassist.HaskellCAProcessor;
import net.sf.eclipsefp.haskell.ui.editor.codeassist.ICompletionContextFactory;
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
		
		ICompletionEngine engine = createMock(ICompletionEngine.class);
		expect(engine.computeProposals((HaskellCompletionContext) anyObject()))
				.andReturn(new ICompletionProposal[] {proposalResult});
		replay(engine);
		
		ICompletionContextFactory factory = createMock(ICompletionContextFactory.class);
		expect(factory.createContext((ITextViewer) anyObject(), anyInt()))
			.andReturn((HaskellCompletionContext) null);
		replay(factory);
		
		HaskellCAProcessor processor = new HaskellCAProcessor(engine, factory);
		ICompletionProposal[] props = processor.computeCompletionProposals(
				                          new StubViewer(input), inputOffset);
		
		assertNotNull(props);
		assertContains(proposalResult, props);
		
		verify(engine, factory);
	}

}
