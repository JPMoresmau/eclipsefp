/**
 * Copyright (c) 2012 by JP Moresmau
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
 * The shortcut to launch a HTF test suite
 * @author JP Moresmau
 *
 */
public class HTFLaunchShortcut extends ExecutableTestSuiteLaunchShortcut {

  @Override
  public List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza ) throws CoreException {
    return HTFLaunchOperation.findConfiguration(project,stanza);
  }

  @Override
  public IExecutableTestSuiteLaunchOperation getLaunchOperation() {
    return new HTFLaunchOperation();
  }
}
