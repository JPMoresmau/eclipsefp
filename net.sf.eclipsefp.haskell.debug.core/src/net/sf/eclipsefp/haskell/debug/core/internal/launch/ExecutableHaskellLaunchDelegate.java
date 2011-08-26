package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.util.Map;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;

/**
 * launch delegate for Haskell executables executables
 *
 * @author JP Moresmau
 * @author Alejandro Serrano
 *
 */
public class ExecutableHaskellLaunchDelegate extends
    ExecutableOrTestSuiteHaskellLaunchDelegate {

  @Override
  protected String getExtraArguments() {
    return ""; //$NON-NLS-1$
  }

  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch,
      final Map<String, String> processAttribs ) {
    // NOOP

  }

  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch ) {
    // NOOP

  }

  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process ) {
    // NOOP

  }

  @Override
  protected void postProcessFinished() {
    // NOOP

  }

}
