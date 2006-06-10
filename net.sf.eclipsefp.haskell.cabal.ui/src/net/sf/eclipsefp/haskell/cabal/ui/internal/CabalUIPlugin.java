// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/** <p>The main plugin class for the Cabal support UI.</p>
  * 
  * @author The mighty PDE wizard
  */
public class CabalUIPlugin extends AbstractUIPlugin {

  // The shared instance
  private static CabalUIPlugin plugin;

  public CabalUIPlugin() {
    plugin = this;
  }

  public static CabalUIPlugin getDefault() {
    return plugin;
  }
  
  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }
  
  public static void log( final String message, final Throwable throwable ) {
    Status status = new Status( IStatus.ERROR, 
                                getPluginId(), 
                                IStatus.OK, 
                                message, 
                                throwable );
    getDefault().getLog().log( status );
  }
}
