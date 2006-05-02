// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.launch;

import java.util.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.debug.core.*;

import de.leiffrenzel.fp.haskell.core.launch.ILaunchAttributes;
import de.leiffrenzel.fp.haskell.core.project.HaskellNature;
import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;


/** <p>encapsulates the work involved in finding a launch configuration 
  * (if one exists) for some element and launching it.</p>
  * 
  * @author Leif Frenzel
  */
class ExecutableLaunchOperation extends LaunchOperation {

  void launch( final IResource resource, 
               final IProgressMonitor monitor ) throws CoreException {
    if( resource != null ) {
      IProject project = resource.getProject();
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {
        IFile executable = findExecutable( resource );
        ILaunchConfiguration configuration = getConfiguration( executable );
        if( configuration != null ) {
          configuration.launch( ILaunchManager.RUN_MODE, monitor );
        }
      }
    }
  }

  
  // helping methods
  //////////////////
  
  private IFile findExecutable( final IResource res ) throws CoreException {
    IFile result = ResourceUtil.getProjectExecutable( res.getProject() );
    if( result == null ) {
      String msg =   "Could not determine executable for selection: " 
                   + res.getName();
      Status status = new Status( IStatus.ERROR, 
                                  HaskellUIPlugin.getPluginId(),
                                  IStatus.ERROR, 
                                  msg, 
                                  null );
      throw new CoreException( status );
    }
    return result;
  }

  private ILaunchConfiguration getConfiguration( final IFile file ) 
                                                          throws CoreException {
    List configurations = findConfiguration( file );    
    ILaunchConfiguration result = null;
    int count = configurations.size();
    if( count < 1 ) {
      // If there are no existing configs associated with the ICompilationUnit, 
      // create one.      
      result = createConfiguration( file );
    } else if( count == 1 ) {
      // If there is exactly one config associated with the ICompilationUnit, 
      // return it.      
      result = ( ILaunchConfiguration )configurations.get( 0 );
    } else {
      // Otherwise, if there is more than one config associated with the 
      // ICompilationUnit, prompt the user to choose one.      
      result = chooseConfiguration( configurations );
    }
    return result;
  }

  private ILaunchConfiguration createConfiguration( final IFile executable ) 
                                                          throws CoreException {
    
    ILaunchConfigurationType configType = getConfigType();
    String id = createConfigId( executable );
    ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, id );
    String exePath = getExePath( executable );
    wc.setAttribute( ILaunchAttributes.EXECUTABLE, exePath );
    String projectName = ILaunchAttributes.PROJECT_NAME;
    wc.setAttribute( projectName, executable.getProject().getName() );
    return wc.doSave();   
  }

  private String getExePath( final IFile executable ) {
    return executable.getLocation().toOSString();
  }

  private List findConfiguration( final IFile file ) throws CoreException {
    List result = Collections.EMPTY_LIST;
    ILaunchConfiguration[] configurations = getConfigurations();
    result = new ArrayList( configurations.length ); 
    for( int i = 0; i < configurations.length; i++ ) {
      ILaunchConfiguration configuration = configurations[ i ];
      String exePath = getExePath( file );
      String projectName = file.getProject().getName();
      if( getExePath( configuration ).equals( exePath )
          && getProjectName( configuration ).equals( projectName ) ) {
        result.add( configuration );
      }
    }
    return result;
  }

  private String createConfigId( final IFile file ) {
    String name = file.getName();
    return getLaunchManager().generateUniqueLaunchConfigurationNameFrom( name );
  }
}