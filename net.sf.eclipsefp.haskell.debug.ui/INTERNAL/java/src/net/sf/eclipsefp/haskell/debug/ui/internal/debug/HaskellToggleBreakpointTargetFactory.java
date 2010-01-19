package net.sf.eclipsefp.haskell.debug.ui.internal.debug;

import java.util.HashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTarget;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTargetFactory;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPart;

/**
 * Not sure this is really needed in anything
 * @author JP Moresmau
 *
 */
public class HaskellToggleBreakpointTargetFactory implements
    IToggleBreakpointsTargetFactory {

  public IToggleBreakpointsTarget createToggleTarget( final String targetID ) {
    if (HaskellDebugCore.ID_HASKELL_DEBUG_MODEL.equals( targetID )){
      return new HaskellLineBreakpointAdapter();
    }
    return null;
  }

  public String getDefaultToggleTarget( final IWorkbenchPart part,
      final ISelection selection ) {
   if (HaskellLineBreakpointAdapter.getEditor( part )!=null){
     return HaskellDebugCore.ID_HASKELL_DEBUG_MODEL;
   }
   return null;
  }

  public String getToggleTargetDescription( final String targetID ) {
    if (HaskellDebugCore.ID_HASKELL_DEBUG_MODEL.equals( targetID )){
      return UITexts.breakpoint_toggle_description;
    }
    return null;
  }

  public String getToggleTargetName( final String targetID ) {
    if (HaskellDebugCore.ID_HASKELL_DEBUG_MODEL.equals( targetID )){
      return UITexts.breakpoint_toggle;
    }
    return null;
  }

  public Set<String> getToggleTargets( final IWorkbenchPart part, final ISelection selection ) {
    Set<String> ret=new HashSet<String>();
    if (HaskellLineBreakpointAdapter.getEditor( part )!=null){
      ret.add(HaskellDebugCore.ID_HASKELL_DEBUG_MODEL);
    }
    return ret;
  }

}
