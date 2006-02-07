package net.sf.eclipsefp.haskell.ui.test.editor.codeassist;

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

		HaskellCAProcessor processor = new HaskellCAProcessor(engine);
		processor.computeCompletionProposals(new StubViewer(input), inputOffset);
		
		engine.verify();
	}

}
