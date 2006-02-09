package net.sf.eclipsefp.haskell.ui.test.editor.codeassist.doubles;

import org.eclipse.jface.text.contentassist.ICompletionProposal;

import junit.framework.Assert;
import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;

public class MockCompletionEngine extends CompletionEngine{

	private int fExpectedOffset;
	private int fTimesCalled = 0;
	private ICompletionProposal[] fOutput;

	public void setExpectedOffset(int offset) {
		fExpectedOffset = offset;
	}

	@Override
	public ICompletionProposal[] complete(ICompilationUnit unit, int offset) {
		fTimesCalled ++;
		Assert.assertNotNull(unit);
		Assert.assertEquals(fExpectedOffset, offset);
		return fOutput;
	}

	public void verify() {
		Assert.assertEquals("Wrong number of calls", 1, fTimesCalled);
	}

	public void setOutput(ICompletionProposal[] output) {
		fOutput = output;
	}
	
}
