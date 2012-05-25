/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.util.Map;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;


/**
 * Launch delegate for yesod server
 * @author JP Moresmau
 *
 */
public class YesodLaunchDelegate extends AbstractHaskellLaunchDelegate {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#postProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.debug.core.model.IProcess)
   */
  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process )  {
    // NOOP

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#preProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, java.util.Map)
   */
  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final Map<String, String> processAttribs )
      {
 // NOOP

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#preProcessDefinitionCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch)
   */
  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch )
       {
 // NOOP

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#postProcessFinished()
   */
  @Override
  protected void postProcessFinished(final ILaunchConfiguration configuration) {
 // NOOP

  }

}
