package net.sf.eclipsefp.haskell.core.test.halamo;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.createNiceMock;
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
	private WorkspaceChangeMonitor fMonitor;

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
		IResourceChangeListener fstMonitor = createNiceMock(IResourceChangeListener.class);
		IResourceChangeListener sndMonitor = createNiceMock(IResourceChangeListener.class);
		expect(getFactory().createProjectChangeMonitor((IProject) anyObject())).
			andReturn(fstMonitor);
		expect(getFactory().createProjectChangeMonitor((IProject) anyObject())).
			andReturn(sndMonitor);
		replay(getFactory());
		
		TestHaskellProject fstPrj = new TestHaskellProject("fisrt-project");
		TestHaskellProject sndPrj = new TestHaskellProject("second-project");
		
		fstMonitor.resourceChanged((IResourceChangeEvent) anyObject());
		replay(fstMonitor, sndMonitor);
		
		fstPrj.createSourceFile("Factorial.hs", "module Factorial where\n\n");
		
		verify(getFactory(), fstMonitor, sndMonitor);
		
		fstPrj.destroy();
		sndPrj.destroy();
	}
	
	public void testCreatesProjectMonitorsForLoadedWorkspace()
		throws CoreException {
		TestHaskellProject prj = new TestHaskellProject("myproject");

		IProjectChangeMonitorFactory factory =
			createMock(IProjectChangeMonitorFactory.class); 
		expect(factory.createProjectChangeMonitor(prj.getPlatformProject())).
			andReturn(createNiceMock(IResourceChangeListener.class));
		replay(factory);
		
		WorkspaceChangeMonitor mon = new WorkspaceChangeMonitor(factory);
		mon.observeChangesOn(getWorkspace());
		
		getWorkspace().removeResourceChangeListener(mon);

		verify(factory);
	}
	
	//TODO do not watch changes on non-haskell projects 

	private IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	private IProjectChangeMonitorFactory getFactory() {
		if (fFactory == null) {
			fFactory = createNiceMock(IProjectChangeMonitorFactory.class); 
			fMonitor = new WorkspaceChangeMonitor(getFactory());
			fMonitor.observeChangesOn(getWorkspace());			
		}
		return fFactory;
	}

	@Override
	protected void tearDown() throws Exception {
		if (fMonitor != null) {
			getWorkspace().removeResourceChangeListener(fMonitor);
		}
	}
	
}
