package net.sf.eclipsefp.haskell.debug.ui.internal.debug;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTarget;

/**
 * factory for breakpoint adapter
 * @author JP Moresmau
 *
 */
public class HaskellBreakpointAdapterFactory implements IAdapterFactory {

  public Object getAdapter( final Object adaptableObject, final Class adapterType ) {
    if (adaptableObject instanceof HaskellEditor) {
      HaskellEditor editorPart = (HaskellEditor) adaptableObject;
      IResource resource = (IResource) editorPart.getEditorInput().getAdapter(IResource.class);
      if (resource != null && FileUtil.hasHaskellExtension( resource )) {
         return new HaskellLineBreakpointAdapter();
      }
    }
    return null;
  }

  public Class<?>[] getAdapterList() {
    return new Class[]{IToggleBreakpointsTarget.class};
  }

}
