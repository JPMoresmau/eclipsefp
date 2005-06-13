// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.launch;

import org.eclipse.debug.ui.*;

/** <p>contains the tabs for setting up a launch configuration for Haskell
  * applications.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellLaunchTabGroup extends AbstractLaunchConfigurationTabGroup {
  

  // interface methods of ILaunchConfigurationTabGroup
  ////////////////////////////////////////////////////

  public void createTabs( final ILaunchConfigurationDialog dialog, 
                          final String mode ) {
    setTabs( new ILaunchConfigurationTab[] { new HaskellArgumentsTab(),
                                             new CommonTab() } );
  }
}