// Copyright (c) 2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.core.internal;

import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.osgi.framework.BundleContext;

/** <p>The main plugin class for the Haskell Debug Core.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellDebugCore extends Plugin {

  public static final String ID_HASKELL_DEBUG_MODEL = "net.sf.eclipsefp.haskell.debug"; //$NON-NLS-1$

  public static final String ID_EXT_POINT_INTERACTIVE_DELEGATES = "interactiveDelegates"; //$NON-NLS-1$
  public static final String ID_EXT_POINT_TEST_LISTENERS = "testListeners"; //$NON-NLS-1$
  /**
   * constant for command history enablement on process
   */
  public static final String PROCESS_COMMAND_HISTORY ="net.sf.eclipse.haskell.debug.commandHistory" ; //$NON-NLS-1$
  // The shared instance
  private static HaskellDebugCore plugin;

  public static HaskellDebugCore getDefault() {
    return plugin;
  }

  public static String getPluginId() {
    return getDefault().getBundle().getSymbolicName();
  }

  public IConfigurationElement[] getExtensions(final String key) {
    IExtensionRegistry registry = Platform.getExtensionRegistry();
    return registry.getConfigurationElementsFor(getPluginId(), key);
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

  public static String getProjectName( final ILaunchConfiguration configuration )
      throws CoreException {
    String att = ILaunchAttributes.PROJECT_NAME;
    return configuration.getAttribute( att, ILaunchAttributes.EMPTY );
  }


  public List<ILaunchConfiguration> listHaskellLaunchConfigurations(final IProject p) throws CoreException{
    List<ILaunchConfiguration> confs=new LinkedList<>();
    for (ILaunchConfiguration c:DebugPlugin.getDefault().getLaunchManager().getLaunchConfigurations()){
      if (c.getType().getContributorName().equals( getPluginId())){
        if (p.getName().equals(getProjectName( c ))){
          confs.add( c );
        }
      }
    }
    return confs;
  }
}
