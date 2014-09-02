package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.EnvironmentTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;


public class ExecutableTestSuiteHaskellLaunchTabGroup extends
    AbstractLaunchConfigurationTabGroup {

  // interface methods of ILaunchConfigurationTabGroup
  ////////////////////////////////////////////////////

  @Override
  public void createTabs( final ILaunchConfigurationDialog dialog,
                          final String mode ) {
    setTabs( new ILaunchConfigurationTab[] { new HaskellArgumentsTab(),
                                             new EnvironmentTab(),
                                             new CommonTab() } );
  }

}
