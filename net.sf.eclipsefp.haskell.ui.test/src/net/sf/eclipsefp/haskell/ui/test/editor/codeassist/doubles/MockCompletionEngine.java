package net.sf.eclipsefp.haskell.ui.test.editor.codeassist.doubles;

import junit.framework.Assert;
import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;

public class MockCompletionEngine extends CompletionEngine{

	private int fExpectedOffset;
	private int fTimesCalled = 0;

	public void setExpectedOffset(int offset) {
		fExpectedOffset = offset;
	}

	@Override
	public String[] complete(ICompilationUnit unit, int offset) {
		fTimesCalled ++;
		Assert.assertEquals(fExpectedOffset, offset);
		return new String[0];
	}

	public void verify() {
		Assert.assertEquals("Wrong number of calls", 1, fTimesCalled);
	}
	
}
