package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;

/**
 * launch delegate for Haskell executables executables
 * @author JP Moresmau
 *
 */
public class ExecutableHaskellLaunchDelegate extends
    AbstractHaskellLaunchDelegate {

  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process )  {
    // NOOP

  }
}
