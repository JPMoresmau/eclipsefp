// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ExecutableHaskellLaunchDelegate;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;

/**
 * <p>
 * encapsulates the work involved in finding a launch configuration (if one
 * exists) for some element and launching it.
 * </p>
 *
 * @author Leif Frenzel
 */
class ExecutableLaunchOperation extends ExecutableOrTestSuiteLaunchOperation implements IExecutableTestSuiteLaunchOperation {
  public static final String EXECUTABLE_CONFIG_TYPE = ExecutableHaskellLaunchDelegate.class.getName();



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
    return EXECUTABLE_CONFIG_TYPE;
  }


  public static List<ILaunchConfiguration> findConfiguration( final IProject project,final PackageDescriptionStanza stanza )
      throws CoreException {
    return ExecutableOrTestSuiteLaunchOperation.findConfiguration( project, EXECUTABLE_CONFIG_TYPE,stanza );
  }


}
