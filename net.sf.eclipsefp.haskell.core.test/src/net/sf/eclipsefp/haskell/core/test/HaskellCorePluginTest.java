package net.sf.eclipsefp.haskell.core.test;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;

import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import junit.framework.TestCase;
import static org.easymock.EasyMock.*;

public class HaskellCorePluginTest extends TestCase {
	
	private BundleContext fStubContext = createNiceMock(BundleContext.class);

	public void testRegistersResourceChangeListener() throws Exception {
		IWorkspace workspace = createMock(IWorkspace.class);
		workspace.addResourceChangeListener((IResourceChangeListener) anyObject(), anyInt());
		expectLastCall().times(1);
		
		expect(workspace.getRoot()).
			andReturn(ResourcesPlugin.getWorkspace().getRoot()).
			atLeastOnce();
		
		replay(workspace, getStubContext());
		
		Plugin corePlugin = new HaskellCorePlugin(workspace);
		corePlugin.start(getStubContext());
		
		verify(workspace);
	}

	@Override
	protected void setUp() throws Exception {
		Bundle stubBundle = createNiceMock(Bundle.class);
		expect(stubBundle.getSymbolicName()).
			andReturn("net.sf.eclipsefp.haskell.core.test.StubBundle").
			anyTimes();
		expect(getStubContext().getBundle()).
			andReturn(stubBundle);
		replay(stubBundle);
	}

	private BundleContext getStubContext() {
		return fStubContext;
	}

}
