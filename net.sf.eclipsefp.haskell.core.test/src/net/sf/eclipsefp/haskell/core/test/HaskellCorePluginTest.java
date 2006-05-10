package net.sf.eclipsefp.haskell.core.test;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.test.internal.doubles.StubBundleContext;

import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Plugin;

import junit.framework.TestCase;
import static org.easymock.EasyMock.*;

public class HaskellCorePluginTest extends TestCase {
	
	public void testRegistersResourceChangeListener() throws Exception {
		IWorkspace workspace = createMock(IWorkspace.class);
		
		workspace.addResourceChangeListener((IResourceChangeListener) anyObject(), anyInt());
		expectLastCall().times(1);
		
		expect(workspace.getRoot()).
			andReturn(ResourcesPlugin.getWorkspace().getRoot());
		
		replay(workspace);
		
		Plugin corePlugin = new HaskellCorePlugin(workspace);
		corePlugin.start(new StubBundleContext());
		
		verify(workspace);
	}

}
