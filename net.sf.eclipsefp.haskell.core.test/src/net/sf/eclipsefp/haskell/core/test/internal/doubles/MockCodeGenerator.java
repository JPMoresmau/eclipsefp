package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import junit.framework.Assert;
import de.leiffrenzel.fp.haskell.core.code.EHaskellCommentStyle;
import de.leiffrenzel.fp.haskell.core.internal.code.CodeGenerator;

public class MockCodeGenerator extends CodeGenerator {
	
	private int fTimesCalled = 0; 
	private String fOutput;
	private EHaskellCommentStyle fExpectedStyle;

	public void setExpectedStyle(EHaskellCommentStyle style) {
		fExpectedStyle = style;
	}
	
	public void setOutput(String output) {
		fOutput = output;
	}
	
	public String createModuleContent(
			final String[] folderNames,
            final String name,
            EHaskellCommentStyle style)
	{
		if (style != fExpectedStyle) {
			Assert.fail("Expecting style " + fExpectedStyle +
					    ", but received " + style + " instead");
		}
		fTimesCalled++;
		return fOutput;
	}
	
	public void verify() {
		Assert.assertEquals(
			"createModuleContent called " + fTimesCalled + " times",
			1, fTimesCalled );
	}

}
