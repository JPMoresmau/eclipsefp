package net.sf.eclipsefp.haskell.core.test.halamo;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.halamo.IProjectChangeMonitorFactory;
import net.sf.eclipsefp.haskell.core.halamo.WorkspaceChangeMonitor;
import net.sf.eclipsefp.test.util.haskell.TestHaskellProject;
import junit.framework.TestCase;

public class WorkspaceChangeMonitor_PDETestCase extends TestCase {
	
	private IProjectChangeMonitorFactory fFactory;

	@Override
	protected void setUp() throws Exception {
		fFactory = createMock(IProjectChangeMonitorFactory.class); 
		WorkspaceChangeMonitor monitor = new WorkspaceChangeMonitor(getFactory());
		
		monitor.observeChangesOn(getWorkspace());
	}

	public void testCreatesChangeMonitorWhenProjectIsCreated()
		throws CoreException
	{
		expect(getFactory().createProjectChangeMonitor((IProject) anyObject())).
			andReturn(null).
			atLeastOnce();
		replay(getFactory());
		
		TestHaskellProject prj = new TestHaskellProject("testing-project");
		prj.destroy();
		
		verify(getFactory());
	}
	
	public void testDelegatesProjectEventsToMonitors() throws CoreException {
		IResourceChangeListener fstMonitor = createMock(IResourceChangeListener.class);
		IResourceChangeListener sndMonitor = createMock(IResourceChangeListener.class);
		expect(getFactory().createProjectChangeMonitor((IProject) anyObject())).
			andReturn(fstMonitor);
		expect(getFactory().createProjectChangeMonitor((IProject) anyObject())).
			andReturn(sndMonitor);
		replay(getFactory());
		
		TestHaskellProject fstPrj = new TestHaskellProject("fisrt-project");
		new TestHaskellProject("second-project");
		
		fstMonitor.resourceChanged((IResourceChangeEvent) anyObject());
		replay(fstMonitor, sndMonitor);
		
		fstPrj.createSourceFile("Factorial.hs", "module Factorial where\n\n");
		
		verify(getFactory(), fstMonitor, sndMonitor);
	}

	private IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	private IProjectChangeMonitorFactory getFactory() {
		return fFactory;
	}
	
}
