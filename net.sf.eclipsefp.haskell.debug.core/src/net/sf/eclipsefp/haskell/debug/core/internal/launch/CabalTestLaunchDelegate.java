/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.util.Map;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;


/**
 * Delegate for "cabal test"
 * @author JP Moresmau
 *
 */
public class CabalTestLaunchDelegate extends
    ExecutableOrTestSuiteHaskellLaunchDelegate {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.ExecutableOrTestSuiteHaskellLaunchDelegate#getExtraArguments()
   */
  @Override
  protected String getExtraArguments() {
    return ""; //$NON-NLS-1$
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#postProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.debug.core.model.IProcess)
   */
  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process )  {
    // noop
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#preProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, java.util.Map)
   */
  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final Map<String, String> processAttribs )     {
    // noop

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#preProcessDefinitionCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch)
   */
  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch )    {
    // noop

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#postProcessFinished(org.eclipse.debug.core.ILaunchConfiguration)
   */
  @Override
  protected void postProcessFinished( final ILaunchConfiguration configuration ) {
    // noop

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate#getExecutableLocation(org.eclipse.debug.core.ILaunchConfiguration)
   */
  @Override
  public IPath getExecutableLocation( final ILaunchConfiguration config ) {
    return new Path(CabalImplementationManager.getCabalExecutable());

  }

}
