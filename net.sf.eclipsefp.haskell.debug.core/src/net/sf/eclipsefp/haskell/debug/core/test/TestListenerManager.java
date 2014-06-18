/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.test;

import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;


/**
 * Helper for test listener extensions
 * @author JP Moresmau
 *
 */
public class TestListenerManager {
  private static Map<String,ITestListener> contribs=null;

  public static Map<String,ITestListener> getContributors(){
    if (contribs==null){
      IConfigurationElement[] elts=HaskellDebugCore.getDefault().getExtensions( HaskellDebugCore.ID_EXT_POINT_TEST_LISTENERS );
      contribs=new HashMap<>();
      for (IConfigurationElement elem:elts){
        try {
          Object o = elem.createExecutableExtension(HaskellCorePlugin.ATT_CLASS);
          contribs.put(o.getClass().getName(), (ITestListener )o);
        } catch (CoreException cex) {
          HaskellCorePlugin.log( cex );
        }
      }
    }
    return contribs;
  }
}
