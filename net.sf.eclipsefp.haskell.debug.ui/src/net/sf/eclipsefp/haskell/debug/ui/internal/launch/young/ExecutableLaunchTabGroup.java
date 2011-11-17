// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

/** <p>contains the tabs for setting up a launch configuration for Haskell
  * applications.</p>
  *
  * @author Leif Frenzel
  */
public class ExecutableLaunchTabGroup extends AbstractLaunchConfigurationTabGroup {


  // interface methods of ILaunchConfigurationTabGroup
  ////////////////////////////////////////////////////

  public void createTabs( final ILaunchConfigurationDialog dialog,
                          final String mode ) {
    setTabs( new ILaunchConfigurationTab[] { new HaskellArgumentsTab(),
                                             new CommonTab() } );
  }
}