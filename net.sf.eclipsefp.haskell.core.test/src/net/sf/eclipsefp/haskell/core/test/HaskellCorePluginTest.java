package net.sf.eclipsefp.haskell.core.test;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.test.internal.doubles.MockWorkspace;
import net.sf.eclipsefp.haskell.core.test.internal.doubles.StubBundleContext;

import org.eclipse.core.runtime.Plugin;

import junit.framework.TestCase;

public class HaskellCorePluginTest extends TestCase {
	
	public void testRegistersResourceChangeListener() throws Exception {
		//TODO use a mock library here
		MockWorkspace workspace = new MockWorkspace();
		Plugin corePlugin = new HaskellCorePlugin(workspace);
		corePlugin.start(new StubBundleContext());
		
		workspace.verify();
	}

}
