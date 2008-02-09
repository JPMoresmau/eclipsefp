package net.sf.eclipsefp.haskell.core.test.internal.util;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import junit.framework.TestCase;

public class ResourceUtil_Test extends TestCase {
	
	public void testExtractModuleNameFromUnliterateFileName() {
		assertEquals("MyModule", ResourceUtil.getModuleName("MyModule.hs"));
	}

	public void testExtractModuleNameFromLiterateFileName() {
		assertEquals("MyModule", ResourceUtil.getModuleName("MyModule.lhs"));
	}
}
