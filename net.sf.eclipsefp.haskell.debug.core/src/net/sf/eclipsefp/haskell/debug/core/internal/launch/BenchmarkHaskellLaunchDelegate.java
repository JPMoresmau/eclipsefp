/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.util.Map;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;


/**
 *
 * @author JP Moresmau
 *
 */
public class BenchmarkHaskellLaunchDelegate extends ExecutableOrTestSuiteHaskellLaunchDelegate {

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
  protected void postProcessFinished(final ILaunchConfiguration configuration) {
  // NOOP

  }


}
