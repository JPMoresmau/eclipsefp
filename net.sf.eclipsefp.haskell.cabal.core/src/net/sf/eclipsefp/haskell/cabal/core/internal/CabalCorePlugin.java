// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.core.internal;

import org.eclipse.core.runtime.Plugin;

/** <p>The main plugin class for the Cabal support core.</p>
  * 
  * @author The mighty PDE wizard
  */
public class CabalCorePlugin extends Plugin {

  // The shared instance
  private static CabalCorePlugin plugin;

  public CabalCorePlugin() {
    plugin = this;
  }

  public static CabalCorePlugin getDefault() {
    return plugin;
  }
}
