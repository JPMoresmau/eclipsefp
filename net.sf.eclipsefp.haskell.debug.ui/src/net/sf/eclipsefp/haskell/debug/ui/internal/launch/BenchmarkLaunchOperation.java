/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.BenchmarkHaskellLaunchDelegate;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;


/**
 * Launch operation for benchmarks
 * @author JP Moresmau
 *
 */
public class BenchmarkLaunchOperation  extends ExecutableOrTestSuiteLaunchOperation implements IExecutableTestSuiteLaunchOperation {
  public static final String BENCHMARK_CONFIG_TYPE = BenchmarkHaskellLaunchDelegate.class.getName();



  // helping methods
  //////////////////




  @Override
  protected String getConfigTypeName() {
    return BENCHMARK_CONFIG_TYPE;
  }


  public static List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza )
      throws CoreException {
    return ExecutableOrTestSuiteLaunchOperation.findConfiguration( project, BENCHMARK_CONFIG_TYPE,stanza );
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.ExecutableOrTestSuiteLaunchOperation#getExecutables(org.eclipse.core.resources.IProject)
   */
  @Override
  protected Map<String, IFile> getExecutables( final IProject project ) {
    return ResourceUtil.getProjectBenchmarks( project );
  }

}
