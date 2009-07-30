package net.sf.eclipsefp.haskell.core.halamo;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.checkOrder;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.resetToDefault;
import static org.easymock.EasyMock.verify;
import net.sf.eclipsefp.haskell.core.internal.util.TestHaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithPreferences;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

public class WorkspaceChangeMonitor_PDETest extends TestCaseWithPreferences {

	private IProjectChangeMonitorFactory fFactory;
	private WorkspaceChangeMonitor fMonitor;

	public void testCreatesChangeMonitorWhenProjectIsCreated()
		throws CoreException
	{
		IProjectChangeMonitorFactory factory = getFactory();
    expect(factory.createProjectChangeMonitor((IProject) anyObject())).
		  andReturn( createMock(IResourceChangeListener.class) );
		replay(factory);

		TestHaskellProject prj = new TestHaskellProject("createsChangeMonitorProject", getCorePrefs());
		prj.destroy();

		verify(factory);
	}

	public void testDelegatesProjectEventsToCorrespondingMonitor() throws CoreException {
		IResourceChangeListener fstMonitor = createNiceMock(IResourceChangeListener.class);
		IResourceChangeListener sndMonitor = createNiceMock(IResourceChangeListener.class);
		expect(getFactory().createProjectChangeMonitor((IProject) anyObject())).
			andReturn(fstMonitor);
		expect(getFactory().createProjectChangeMonitor((IProject) anyObject())).
			andReturn(sndMonitor);
		replay(getFactory());

		TestHaskellProject fstPrj = new TestHaskellProject("first-project", getCorePrefs());
		TestHaskellProject sndPrj = new TestHaskellProject("second-project", getCorePrefs());

		try {
			fstMonitor.resourceChanged((IResourceChangeEvent) anyObject());
			replay(fstMonitor, sndMonitor);
			fstPrj.createSourceFile("Factorial.hs",
					"module Factorial where\n\n");
			verify(getFactory(), fstMonitor, sndMonitor);
		} finally {
			fstPrj.destroy();
			sndPrj.destroy();
		}
	}

	public void testCreatesProjectMonitorsForLoadedWorkspace()
		throws CoreException {
		TestHaskellProject prj = new TestHaskellProject("myproject", getCorePrefs());

		IProjectChangeMonitorFactory factory =
			createMock(IProjectChangeMonitorFactory.class);
		expect(factory.createProjectChangeMonitor(prj.getPlatformProject()))
			.andReturn(createNiceMock(IResourceChangeListener.class));
		expect(factory.createProjectChangeMonitor( (IProject ) anyObject() ))
		  .andReturn( createNiceMock(IResourceChangeListener.class) )
			.atLeastOnce(); // projects might already exist in the workspace
		checkOrder(factory, false);
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
		  // WorkspaceChangeMonitor.observeChangesOn() will cause some calls on IProjectChangeMonitorFactory
		  // and expects it to return non-null
			fFactory = createMock(IProjectChangeMonitorFactory.class);
			expect(fFactory.createProjectChangeMonitor( (IProject ) anyObject() ))
			  .andStubReturn( createNiceMock(IResourceChangeListener.class) );
			replay(fFactory);

			fMonitor = new WorkspaceChangeMonitor(fFactory);
			fMonitor.observeChangesOn(getWorkspace());

			resetToDefault(fFactory);
		}
		return fFactory;
	}

	@Override
	protected void tearDown() throws Exception {
		if (fMonitor != null) {
			getWorkspace().removeResourceChangeListener(fMonitor);
		}
		super.tearDown();
	}

}
