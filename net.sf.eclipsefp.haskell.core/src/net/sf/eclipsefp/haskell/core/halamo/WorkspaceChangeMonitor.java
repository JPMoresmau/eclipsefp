package net.sf.eclipsefp.haskell.core.halamo;

import java.util.Hashtable;
import java.util.Map;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;

public class WorkspaceChangeMonitor implements IResourceChangeListener {

	/** the resource change types the ResourceChangeMonitor is interested in. */
	public static final int TYPES = IResourceChangeEvent.PRE_BUILD
			| IResourceChangeEvent.POST_BUILD
			| IResourceChangeEvent.POST_CHANGE
			| IResourceChangeEvent.PRE_DELETE | IResourceChangeEvent.PRE_CLOSE;

	private class DeltaVisitor implements IResourceDeltaVisitor {

		private IResourceChangeEvent fOriginalEvent;

		public DeltaVisitor(IResourceChangeEvent event) {
			fOriginalEvent = event;
		}

		public boolean visit(IResourceDelta delta) {
			if (IResource.PROJECT == delta.getResource().getType()) {
				IProject project = (IProject) delta.getResource();

				if (IResourceDelta.ADDED == delta.getKind()) {
					IResourceChangeListener prjMon =
						createProjectChangeMonitor(project);
					fProjectMonitors.put(project, prjMon);
					return false;
				}
				
				IResourceChangeListener prjMon = fProjectMonitors.get(project);
				if (null != prjMon) {
					prjMon.resourceChanged(fOriginalEvent);
				}
				
				return false;
			}
			
			return true;
		}


	}
	
	private IProjectChangeMonitorFactory fProjectMonitorFactory; 

	private Map<IProject, IResourceChangeListener> fProjectMonitors =
		new Hashtable<IProject, IResourceChangeListener>();

	public WorkspaceChangeMonitor() {
		this(new IProjectChangeMonitorFactory() {
			public IResourceChangeListener createProjectChangeMonitor(
					IProject project)
			{
				return new ProjectChangeMonitor(project);
			}
		});
	}

	public WorkspaceChangeMonitor(IProjectChangeMonitorFactory factory) {
		fProjectMonitorFactory = factory;
	}

	public void resourceChanged(IResourceChangeEvent event) {
		try {
			event.getDelta().accept(new DeltaVisitor(event));
		} catch (CoreException exc) {
			HaskellCorePlugin.log("Error when exploring workspace delta", exc);
		}
	}

	public IResourceChangeListener createProjectChangeMonitor(IProject project) {
		return fProjectMonitorFactory.createProjectChangeMonitor(project);
	}

	public void observeChangesOn(IWorkspace workspace) {
		workspace.addResourceChangeListener(this, TYPES);
	}

}
