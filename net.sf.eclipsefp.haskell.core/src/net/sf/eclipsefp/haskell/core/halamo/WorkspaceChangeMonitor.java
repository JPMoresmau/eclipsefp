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

		private final IResourceChangeEvent fOriginalEvent;

		public DeltaVisitor(final IResourceChangeEvent event) {
			fOriginalEvent = event;
		}

		public boolean visit(final IResourceDelta delta) {
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

	private final IProjectChangeMonitorFactory fProjectMonitorFactory;

	private final Map<IProject, IResourceChangeListener> fProjectMonitors =
		new Hashtable<IProject, IResourceChangeListener>();

	public WorkspaceChangeMonitor() {
		this(new IProjectChangeMonitorFactory() {
			public IResourceChangeListener createProjectChangeMonitor(
					final IProject project)
			{
				return new ProjectChangeMonitor(project);
			}
		});
	}

	public WorkspaceChangeMonitor(final IProjectChangeMonitorFactory factory) {
		fProjectMonitorFactory = factory;
	}

	public void resourceChanged( final IResourceChangeEvent event ) {
    try {
      IResourceDelta delta = event.getDelta();
      if( delta != null ) {
        delta.accept( new DeltaVisitor( event ) );
      }
    } catch( CoreException exc ) {
      HaskellCorePlugin.log( "Error when exploring workspace delta", exc ); //$NON-NLS-1$
    }
  }

	public IResourceChangeListener createProjectChangeMonitor(final IProject project) {
		return fProjectMonitorFactory.createProjectChangeMonitor(project);
	}

	public void observeChangesOn(final IWorkspace workspace) {
		createProjectMonitorsFor(workspace);
		workspace.addResourceChangeListener(this, TYPES);
	}

	private void createProjectMonitorsFor(final IWorkspace workspace) {
		for(IProject project : workspace.getRoot().getProjects()) {
			fProjectMonitors.put(project, createProjectChangeMonitor(project));
		}
	}

}
