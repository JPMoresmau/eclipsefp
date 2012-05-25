/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.yesod;

import net.sf.eclipsefp.haskell.debug.ui.internal.launch.HaskellArgumentsTab;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;


/**
 * Configuration Tab for Yesod launch configuration
 * @author JP Moresmau
 *
 */
public class YesodLaunchTabGroup extends AbstractLaunchConfigurationTabGroup {

  /* (non-Javadoc)
   * @see org.eclipse.debug.ui.ILaunchConfigurationTabGroup#createTabs(org.eclipse.debug.ui.ILaunchConfigurationDialog, java.lang.String)
   */
  @Override
  public void createTabs( final ILaunchConfigurationDialog arg0, final String arg1 ) {
    setTabs( new ILaunchConfigurationTab[] {
        new HaskellArgumentsTab(),
        new CommonTab() } );

  }

}
