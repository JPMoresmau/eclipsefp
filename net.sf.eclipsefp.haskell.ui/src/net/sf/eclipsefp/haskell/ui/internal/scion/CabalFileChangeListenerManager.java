package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.util.ArrayList;
import java.util.Collection;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

/**
 * <p>Utility to retrieve cabal file change listener</p>
  *
  * @author JP Moresmau
 */
public class CabalFileChangeListenerManager {
  private static Collection<CabalFileChangeListener> listeners=null;

  public static Collection<CabalFileChangeListener> getListeners(){
    if (listeners==null){
      IConfigurationElement[] elts=HaskellUIPlugin.getDefault().getExtensions( HaskellUIPlugin.ID_EXT_CabalChangeListener );
      listeners=new ArrayList<CabalFileChangeListener>(elts.length);
      for (IConfigurationElement elem:elts){
        try {
          Object o = elem.createExecutableExtension(HaskellCorePlugin.ATT_CLASS);
          listeners.add( (CabalFileChangeListener )o);
        } catch (CoreException cex) {
          HaskellCorePlugin.log( cex );
        }
      }
    }
    return listeners;
  }
}
