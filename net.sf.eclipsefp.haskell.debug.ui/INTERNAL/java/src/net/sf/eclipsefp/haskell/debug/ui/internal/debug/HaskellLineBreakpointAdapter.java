package net.sf.eclipsefp.haskell.debug.ui.internal.debug;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.debug.HaskellBreakpoint;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTarget;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.texteditor.ITextEditor;


public class HaskellLineBreakpointAdapter implements IToggleBreakpointsTarget {

  /**
   * Returns the editor being used to edit a PDA file, associated with the
   * given part, or <code>null</code> if none.
   *
   * @param part workbench part
   * @return the editor being used to edit a PDA file, associated with the
   * given part, or <code>null</code> if none
   */
  private ITextEditor getEditor(final IWorkbenchPart part) {
    if (part instanceof ITextEditor) {
      ITextEditor editorPart = (ITextEditor) part;
      IResource resource = (IResource) editorPart.getEditorInput().getAdapter(IResource.class);
      if(resource != null && ResourceUtil.hasHaskellExtension( resource )) {
         return editorPart;
      }
    }
    return null;
  }

  public boolean canToggleLineBreakpoints( final IWorkbenchPart part,
      final ISelection selection ) {
    return getEditor(part) != null;
  }

  public boolean canToggleMethodBreakpoints( final IWorkbenchPart part,
      final ISelection selection ) {
    // TODO Auto-generated method stub
    return false;
  }

  public boolean canToggleWatchpoints( final IWorkbenchPart part, final ISelection selection ) {
    return false;
  }

  public void toggleLineBreakpoints( final IWorkbenchPart part, final ISelection selection )
      throws CoreException {
    ITextEditor textEditor = getEditor(part);
    if (textEditor != null) {
      IResource resource = (IResource) textEditor.getEditorInput().getAdapter(IResource.class);
      ITextSelection textSelection = (ITextSelection) selection;
      int lineNumber = textSelection.getStartLine();
      IBreakpoint[] breakpoints = DebugPlugin.getDefault().getBreakpointManager().getBreakpoints(HaskellDebugCore.ID_HASKELL_DEBUG_MODEL);
      for (int i = 0; i < breakpoints.length; i++) {
        IBreakpoint breakpoint = breakpoints[i];
        if (resource.equals(breakpoint.getMarker().getResource())) {
          if (((ILineBreakpoint)breakpoint).getLineNumber() == (lineNumber + 1)) {
            // remove
            breakpoint.delete();
            return;
          }
        }
      }
      // create line breakpoint (doc line numbers start at 0)
      HaskellBreakpoint lineBreakpoint = new HaskellBreakpoint(resource, lineNumber + 1);
      DebugPlugin.getDefault().getBreakpointManager().addBreakpoint(lineBreakpoint);
    }

  }

  public void toggleMethodBreakpoints( final IWorkbenchPart part, final ISelection selection )
      throws CoreException {
    // TODO Auto-generated method stub

  }

  public void toggleWatchpoints( final IWorkbenchPart part, final ISelection selection )
      throws CoreException {
    // TODO Auto-generated method stub

  }

}
