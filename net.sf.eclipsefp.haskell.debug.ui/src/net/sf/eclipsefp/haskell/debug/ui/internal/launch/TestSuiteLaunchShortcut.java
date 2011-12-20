// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;


/** <p>Shortcut to run tests from the 'Run' action set.</p>
  *
  * @author Alejandro Serrano
  */
public class TestSuiteLaunchShortcut extends ExecutableTestSuiteLaunchShortcut {

  @Override
  public List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza ) throws CoreException {
    return TestSuiteLaunchOperation.findConfiguration(project,stanza);
  }

  @Override
  public IExecutableTestSuiteLaunchOperation getLaunchOperation() {
    return new TestSuiteLaunchOperation();
  }
}