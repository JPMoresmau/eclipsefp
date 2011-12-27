// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.compat.ILaunchManagerCompat;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ExecutableProfilingHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.osgi.util.NLS;

/**
 * <p>
 * encapsulates the work involved in finding a launch configuration (if one
 * exists) for some element and launching it.
 * </p>
 *
 * @author Alejandro Serrano
 */
class ExecutableProfilingLaunchOperation extends ExecutableOrTestSuiteLaunchOperation implements IExecutableTestSuiteLaunchOperation {
  public static final String EXECUTABLE_PROF_CONFIG_TYPE = ExecutableProfilingHaskellLaunchDelegate.class.getName();

  @Override
  protected String getConfigTypeName() {
    return EXECUTABLE_PROF_CONFIG_TYPE;
  }

  public static List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza )
      throws CoreException {
    return ExecutableOrTestSuiteLaunchOperation.findConfiguration( project, EXECUTABLE_PROF_CONFIG_TYPE,stanza );
  }

  @Override
  protected String createConfigId( final String stanza ) {
   // String name = file.getName();
    String name = NLS.bind( UITexts.profiling_name, stanza );
    // FIXME: Remove when Galileo is no longer supported.
    ILaunchManager mgr = getLaunchManager();
    return ILaunchManagerCompat.generateLaunchConfigurationName(mgr, name);
  }
}
