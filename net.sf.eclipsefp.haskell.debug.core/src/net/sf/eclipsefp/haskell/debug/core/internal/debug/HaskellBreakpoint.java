package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.LineBreakpoint;
import org.eclipse.osgi.util.NLS;

/**
 * breakpoint on haskell source file
 * @author JP Moresmau
 *
 */
public class HaskellBreakpoint extends LineBreakpoint {


  public HaskellBreakpoint() {
    //noop
  }

  /**
   *
   * @param resource file on which to set the breakpoint
   * @param lineNumber 1-based line number of the breakpoint
   * @throws CoreException if unable to create the breakpoint
   */
  public HaskellBreakpoint(final IResource resource, final int lineNumber)
      throws CoreException {
    IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
      public void run(final IProgressMonitor monitor) throws CoreException {
        IMarker marker = resource
            .createMarker("net.sf.eclipsefp.haskell.debug.core.breakpoint"); //$NON-NLS-1$
        setMarker(marker);
        marker.setAttribute(IBreakpoint.ENABLED, Boolean.TRUE);
        marker.setAttribute(IBreakpoint.PERSISTED, Boolean.TRUE);
        marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
        marker.setAttribute(IBreakpoint.ID, getModelIdentifier());
        marker.setAttribute(IMarker.MESSAGE, NLS.bind( CoreTexts.breakpoint_message, resource.getName(),String.valueOf(lineNumber) ));
      }
    };
    run(getMarkerRule(resource), runnable);
  }

  public String getModelIdentifier() {
    return HaskellDebugCore.ID_HASKELL_DEBUG_MODEL;
  }

}
