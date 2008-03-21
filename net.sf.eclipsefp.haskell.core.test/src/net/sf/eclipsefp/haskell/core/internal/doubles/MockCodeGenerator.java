package net.sf.eclipsefp.haskell.core.internal.doubles;

import junit.framework.Assert;
import net.sf.eclipsefp.haskell.core.code.EHaskellCommentStyle;
import net.sf.eclipsefp.haskell.core.internal.code.CodeGenerator;

public class MockCodeGenerator extends CodeGenerator {

	private int fTimesCalled = 0;
	private String fOutput;
	private EHaskellCommentStyle fExpectedStyle;

	public void setExpectedStyle(final EHaskellCommentStyle style) {
		fExpectedStyle = style;
	}

	public void setOutput(final String output) {
		fOutput = output;
	}

	@Override
  public String createModuleContent(
			final String[] folderNames,
            final String name,
            final EHaskellCommentStyle style)
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
