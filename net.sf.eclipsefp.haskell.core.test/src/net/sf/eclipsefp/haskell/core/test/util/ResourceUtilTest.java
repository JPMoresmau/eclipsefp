package net.sf.eclipsefp.haskell.core.test.util;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import junit.framework.TestCase;

public class ResourceUtilTest extends TestCase {
	
	public void testExtractModuleNameFromUnliterateFileName() {
		assertEquals("MyModule", ResourceUtil.getModuleName("MyModule.hs"));
	}

	public void testExtractModuleNameFromLiterateFileName() {
		assertEquals("MyModule", ResourceUtil.getModuleName("MyModule.lhs"));
	}
}
