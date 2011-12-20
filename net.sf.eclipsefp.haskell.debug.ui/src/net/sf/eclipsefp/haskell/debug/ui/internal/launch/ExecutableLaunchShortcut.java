// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;


/** <p>Shortcut to launch Haskell applications from the 'Run' action set.</p>
  *
  * @author Leif Frenzel
  */
public class ExecutableLaunchShortcut extends ExecutableTestSuiteLaunchShortcut {

  @Override
  public List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza ) throws CoreException {
    return ExecutableLaunchOperation.findConfiguration(project,stanza);
  }

  @Override
  public IExecutableTestSuiteLaunchOperation getLaunchOperation() {
    return new ExecutableLaunchOperation();
  }
}