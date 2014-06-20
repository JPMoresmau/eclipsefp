package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

/**
 * Keep the list of interactive delegates registered in all plugins
 * @author JP Moresmau
 *
 */
public class InteractiveDelegateManager {
  private static Map<String,IInteractiveLaunchOperationDelegate> contribs=null;

  public static Map<String,IInteractiveLaunchOperationDelegate> getContributors(){
    if (contribs==null){
      IConfigurationElement[] elts=HaskellDebugCore.getDefault().getExtensions( HaskellDebugCore.ID_EXT_POINT_INTERACTIVE_DELEGATES );
      contribs=new HashMap<>();
      for (IConfigurationElement elem:elts){
        try {
          Object o = elem.createExecutableExtension(HaskellCorePlugin.ATT_CLASS);
          contribs.put(o.getClass().getName(), (IInteractiveLaunchOperationDelegate )o);
        } catch (CoreException cex) {
          HaskellCorePlugin.log( cex );
        }
      }
    }
    return contribs;
  }
}
