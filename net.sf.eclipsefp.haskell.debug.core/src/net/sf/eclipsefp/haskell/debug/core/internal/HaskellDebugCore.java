// Copyright (c) 2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.core.internal;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.BundleContext;

/** <p>The main plugin class for the Haskell Debug Core.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellDebugCore extends Plugin {

  public static final String ID_HASKELL_DEBUG_MODEL = "net.sf.eclipsefp.haskell.debug"; //$NON-NLS-1$

  // The shared instance
  private static HaskellDebugCore plugin;

  public static HaskellDebugCore getDefault() {
    return plugin;
  }

  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
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

  public static void log( final String message, final Throwable thr ) {
    String id = getPluginId();
    Status status = new Status( IStatus.ERROR, id, IStatus.OK, message, thr );
    getDefault().getLog().log( status );
  }
}
