/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.compat.ILaunchManagerCompat;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
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
 * common code for launching executables and test suite
 * @author JP Moresmau
 *
 */
public abstract class ExecutableOrTestSuiteLaunchOperation extends LaunchOperation {

  public void launch( final IResource resource, final IProgressMonitor monitor,final PackageDescriptionStanza stanza  )
  throws CoreException {
  if( resource != null ) {
    IProject project = resource.getProject();
    if(ResourceUtil.hasHaskellNature (project) ) {
      Map<String,IFile> executables=getExecutables(project);
      ILaunchConfiguration configuration = getConfiguration( project,executables,stanza );
      if( configuration != null ) {
        configuration.launch( ILaunchManager.RUN_MODE, monitor );
      }
    }
  }
}

  protected Map<String,IFile> getExecutables(final IProject project){
    /** offer the option to run a test executable without any special options and processing **/
    Map<String,IFile> m=new HashMap<>();
    m.putAll(ResourceUtil.getProjectExecutables( project ));
    m.putAll(ResourceUtil.getProjectTestSuites( project ));
    m.putAll(ResourceUtil.getProjectBenchmarks( project ));
    return m;
  }

  protected ILaunchConfiguration getConfiguration( final IProject project,
      final Map<String,IFile> executables ,final PackageDescriptionStanza stanza) throws CoreException {
    List<ILaunchConfiguration> configurations = findConfiguration( project,getConfigTypeName(),stanza );
    // match existing configurations with executables
    Set<String> exesExisting=new HashSet<> ();
    for (Iterator<ILaunchConfiguration> it=configurations.iterator();it.hasNext();){
        ILaunchConfiguration c=it.next();
        String exe=getExePath( c );
        if (exe!=null && exe.length()>0 && new File(exe).exists()){
           exesExisting.add( exe );
        } else {
          it.remove();
        }
    }
    ILaunchConfiguration result = null;
    /*IFile exe=null;
    if (stanza!=null){
      exe=ResourceUtil.getExecutableLocation( project, stanza.getName() );
    }*/


    for( String s:executables.keySet() ) {
      String exe1=getExePath( executables.get( s ) );
      if (!exesExisting.contains(exe1)){
        if (stanza==null || stanza.getName().equals(s)){
          configurations.add( createConfiguration(project, s ) );
        }
      }

    }

    int count = configurations.size();

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

  private ILaunchConfiguration createConfiguration(final IProject proj, final String stanza )
      throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    String id = createConfigId( stanza );
    ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, id );
    wc.setAttribute( ILaunchAttributes.STANZA, stanza );
   // wc.setAttribute( ILaunchAttributes.EXECUTABLE, exePath );
    wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, proj.getLocation().toOSString() );
    wc.setAttribute( ILaunchAttributes.PROJECT_NAME, proj.getName() );
    wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
    return wc.doSave();
  }


  private String getExePath( final IFile executable ) {
    String result = executable.getLocation().toOSString();
    if( !new File( result ).exists() ) {
      String msg = "Could not locate the project executable - supposed to be " + result; //$NON-NLS-1$
      HaskellUIPlugin.log( msg, IStatus.ERROR );
    }
    return result;
  }

  public static List<ILaunchConfiguration> findConfiguration(
      final IProject project,final String configTypeName,final PackageDescriptionStanza stanza  ) throws CoreException {
    List<ILaunchConfiguration> result = new LinkedList<>();
    ILaunchConfiguration[] configurations = LaunchOperation
        .getConfigurations( LaunchOperation
            .getConfigType( configTypeName ) );

    IFile exe=null;
    if (stanza!=null){
      exe=ResourceUtil.getExecutableLocation( project, stanza.getName() );
    }
    result = new ArrayList<>( configurations.length );
    for( int i = 0; i < configurations.length; i++ ) {
      ILaunchConfiguration configuration = configurations[ i ];
      String thisProject=configuration.getAttribute( ILaunchAttributes.PROJECT_NAME, (String)null );
      if (project.getName().equals( thisProject )){
          // String exePath = getExePath( file );
          String projectName = project.getName();
          if (exe!=null){
            if (getExePath( configuration ).equals( exe.getLocation().toOSString() )){
              result.add( configuration );
            }
          } else {
            if( getExePath( configuration ).startsWith(
                project.getLocation().toOSString() )
                && HaskellDebugCore.getProjectName( configuration ).equals( projectName ) ) {
              result.add( configuration );
            }
          }
      }
    }
    return result;
  }

  @Override
  protected String createConfigId( final String stanza ) {
    //String name = file.getName();
    // FIXME: Remove when Galileo is no longer supported.
    ILaunchManager mgr = getLaunchManager();
    return ILaunchManagerCompat.generateLaunchConfigurationName( mgr, stanza );
  }
}
