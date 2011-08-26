// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.compat.ILaunchManagerCompat;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ExecutableProfilingHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;

/**
 * <p>
 * encapsulates the work involved in finding a launch configuration (if one
 * exists) for some element and launching it.
 * </p>
 *
 * @author Alejandro Serrano
 */
class ExecutableProfilingLaunchOperation extends LaunchOperation implements IExecutableTestSuiteLaunchOperation {
  public static final String EXECUTABLE_PROF_CONFIG_TYPE = ExecutableProfilingHaskellLaunchDelegate.class.getName();

  public void launch( final IResource resource, final IProgressMonitor monitor )
      throws CoreException {
    if( resource != null ) {
      IProject project = resource.getProject();
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {
        List<IFile> executables=ResourceUtil.getProjectExecutables( project );
        ILaunchConfiguration configuration = getConfiguration( project,executables );
        if( configuration != null ) {
          configuration.launch( ILaunchManager.RUN_MODE, monitor );
        }
      }
    }
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

  private ILaunchConfiguration getConfiguration(final IProject project, final List<IFile> executables )
      throws CoreException {
    List<ILaunchConfiguration> configurations = findConfiguration( project );
    ILaunchConfiguration result = null;
    int count = configurations.size();
    if( count < 1 ) {
      // If there are no existing configs associated with the
      // ICompilationUnit,
      // create one.
      //result = createConfiguration( file );
      for (IFile f:executables){
        configurations.add( createConfiguration( f ) );
      }
      count = configurations.size();
    }


    if( count == 1 ) {
      // If there is exactly one config associated with the
      // ICompilationUnit,
      // return it.
      result = configurations.get( 0 );
    } else {
      // Otherwise, if there is more than one config associated with the
      // ICompilationUnit, prompt the user to choose one.
      result = choose( configurations );
    }
    return result;
  }

  @Override
  protected String getConfigTypeName() {
    return EXECUTABLE_PROF_CONFIG_TYPE;
  }

  private ILaunchConfiguration createConfiguration( final IFile executable )
      throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    String id = createConfigId( executable );
    ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, id );
    String exePath = getExePath( executable );
    wc.setAttribute( ILaunchAttributes.EXECUTABLE, exePath );
    wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, executable.getProject().getLocation().toOSString() );
    String projectName = ILaunchAttributes.PROJECT_NAME;
    wc.setAttribute( projectName, executable.getProject().getName() );
    wc.setAttribute(ILaunchAttributes.SYNC_STREAMS,true);
    return wc.doSave();
  }



  private String getExePath( final IFile executable ) {
    String result = executable.getLocation().toOSString();
    if( !new File( result ).exists() ) {
      String msg
        = "Could not locate the project executable - supposed to be " + result; //$NON-NLS-1$
      HaskellUIPlugin.log( msg, IStatus.ERROR );
    }
    return result;
  }

  public static List<ILaunchConfiguration> findConfiguration( final IProject project )
      throws CoreException {
    List<ILaunchConfiguration> result = new ArrayList<ILaunchConfiguration>();
    ILaunchConfiguration[] configurations =  LaunchOperation.getConfigurations(LaunchOperation.getConfigType( EXECUTABLE_PROF_CONFIG_TYPE ));
    result = new ArrayList<ILaunchConfiguration>( configurations.length );
    for( int i = 0; i < configurations.length; i++ ) {
      ILaunchConfiguration configuration = configurations[ i ];
      //String exePath = getExePath( file );
      String projectName = project.getName();
      if( getExePath( configuration ).startsWith( project.getLocation().toOSString() )
          && getProjectName( configuration ).equals( projectName ) ) {
        result.add( configuration );
      }
    }
    return result;
  }

  private String createConfigId( final IFile file ) {
    String name = file.getName();
    name = name + " (profiling)"; //$NON-NLS-1$
    // FIXME: Remove when Galileo is no longer supported.
    ILaunchManager mgr = getLaunchManager();
    return ILaunchManagerCompat.generateLaunchConfigurationName(mgr, name);
  }
}
