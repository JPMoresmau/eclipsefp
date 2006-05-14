package net.sf.eclipsefp.haskell.core.halamo;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeListener;

public interface IProjectChangeMonitorFactory {

	IResourceChangeListener createProjectChangeMonitor(IProject project);

}
