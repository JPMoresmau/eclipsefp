// Copyright (c) 2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/** <p>The main plugin class for the Haskell Debug UI.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellDebugUI extends AbstractUIPlugin {

  private static HaskellDebugUI plugin;

  public static HaskellDebugUI getDefault() {
    return plugin;
  }


  // interface methods of Activator
  /////////////////////////////////

  @Override
  public void start( final BundleContext context ) throws Exception {
    super.start( context );
    plugin = this;

  }

  @Override
  public void stop( final BundleContext context ) throws Exception {
    plugin = null;
    super.stop( context );
  }
}
