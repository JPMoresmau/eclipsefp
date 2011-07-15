package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;


public interface IExecutableTestSuiteLaunchOperation {
  void launch( final IResource resource, final IProgressMonitor monitor )
      throws CoreException;
}
