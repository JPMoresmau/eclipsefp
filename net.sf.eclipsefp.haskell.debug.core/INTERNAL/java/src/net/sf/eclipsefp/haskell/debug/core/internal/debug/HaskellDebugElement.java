package net.sf.eclipsefp.haskell.debug.core.internal.debug;

import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugElement;

/**
 * Common superclass for debug element
 * @author JP Moresmau
 *
 */
public abstract class HaskellDebugElement implements IDebugElement {
  protected HaskellDebugTarget target;

  public HaskellDebugElement(){
    // NOOP
  }

  public HaskellDebugElement(final HaskellDebugTarget target){
    setTarget( target );
  }

  public HaskellDebugTarget getDebugTarget() {
    return target;
  }


  void setTarget( final HaskellDebugTarget target ) {
    this.target = target;
  }

  public ILaunch getLaunch() {
    return getDebugTarget().getLaunch();
  }

  public String getModelIdentifier() {
    return HaskellDebugCore.ID_HASKELL_DEBUG_MODEL;
  }

  public Object getAdapter( final Class adapter ) {
    if (adapter.isAssignableFrom(this.getClass() )){
      return this;
    }
    return null;
  }

}
