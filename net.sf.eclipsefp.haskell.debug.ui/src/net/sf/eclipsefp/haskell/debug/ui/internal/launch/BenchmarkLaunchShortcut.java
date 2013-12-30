/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;


/**
 * Shortcut for benchmark launch
 * @author JP Moresmau
 *
 */
public class BenchmarkLaunchShortcut  extends ExecutableTestSuiteLaunchShortcut {

  @Override
  public List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza ) throws CoreException {
    return BenchmarkLaunchOperation.findConfiguration(project,stanza);
  }

  @Override
  public IExecutableTestSuiteLaunchOperation getLaunchOperation() {
    return new BenchmarkLaunchOperation();
  }
}
