// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.TestSuiteHaskellLaunchDelegate;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;

/**
 * <p>
 * encapsulates the work involved in finding a launch configuration (if one
 * exists) for some element and launching it.
 * </p>
 *
 * @author Alejandro Serrano
 */
class TestSuiteLaunchOperation extends ExecutableOrTestSuiteLaunchOperation implements IExecutableTestSuiteLaunchOperation {
  public static final String TEST_SUITE_CONFIG_TYPE = TestSuiteHaskellLaunchDelegate.class.getName();

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.ExecutableOrTestSuiteLaunchOperation#getExecutables(org.eclipse.core.resources.IProject)
   */
  @Override
  protected Map<String, IFile> getExecutables( final IProject project ) {
    return ResourceUtil.getProjectTestSuites( project );
  }

  // helping methods
  //////////////////

//  private IFile findExecutable( final IResource res ) throws CoreException {
//    IFile result = null;
//    IFile[] exes = ResourceUtil.getProjectExecutables( res.getProject() );
//    for( IFile exe: exes ) {
//      if( res.equals( exe ) ) {
//        result = exe;
//      }
//    }
//    if( result == null && exes.length == 1 ) {
//      result = exes[ 0 ];
//    }
//    if( result == null ) {
//      String pattern = UITexts.executableLaunchOperations_errorMsg;
//      String msg = NLS.bind( pattern, res.getName() );
//      String pluginId = HaskellUIPlugin.getPluginId();
//      Status status = new Status( IStatus.ERROR, pluginId, 0, msg, null );
//      throw new CoreException( status );
//    }
//    return result;
//  }

  @Override
  protected String getConfigTypeName() {
    return TEST_SUITE_CONFIG_TYPE;
  }

  public static List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza )
      throws CoreException {
    return ExecutableOrTestSuiteLaunchOperation.findConfiguration( project, TEST_SUITE_CONFIG_TYPE,stanza );

  }


}
