package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.compat.ILaunchManagerCompat;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
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

  protected ILaunchConfiguration getConfiguration( final IProject project,
      final List<IFile> executables ) throws CoreException {
    List<ILaunchConfiguration> configurations = findConfiguration( project,getConfigTypeName() );
    // match existing configurations with executables
    Set<String> exesExisting=new HashSet<String> ();
    for (Iterator<ILaunchConfiguration> it=configurations.iterator();it.hasNext();){
        ILaunchConfiguration c=it.next();
        String exe=c.getAttribute( ILaunchAttributes.EXECUTABLE, "" ); //$NON-NLS-1$
        if (exe!=null && exe.length()>0 && new File(exe).exists()){
           exesExisting.add( exe );
        } else {
          it.remove();
        }
    }
    ILaunchConfiguration result = null;
    for( IFile f: executables ) {
      String exe=getExePath( f );
      if (!exesExisting.contains(exe)){
        configurations.add( createConfiguration( f,exe ) );
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

  private ILaunchConfiguration createConfiguration( final IFile executable,final String exePath )
      throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    String id = createConfigId( executable );
    ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, id );
    wc.setAttribute( ILaunchAttributes.EXECUTABLE, exePath );
    wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, executable
        .getProject().getLocation().toOSString() );
    String projectName = ILaunchAttributes.PROJECT_NAME;
    wc.setAttribute( projectName, executable.getProject().getName() );
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
      final IProject project,final String configTypeName ) throws CoreException {
    List<ILaunchConfiguration> result = new LinkedList<ILaunchConfiguration>();
    ILaunchConfiguration[] configurations = LaunchOperation
        .getConfigurations( LaunchOperation
            .getConfigType( configTypeName ) );
    result = new ArrayList<ILaunchConfiguration>( configurations.length );
    for( int i = 0; i < configurations.length; i++ ) {
      ILaunchConfiguration configuration = configurations[ i ];
      // String exePath = getExePath( file );
      String projectName = project.getName();
      if( getExePath( configuration ).startsWith(
          project.getLocation().toOSString() )
          && getProjectName( configuration ).equals( projectName ) ) {
        result.add( configuration );
      }
    }
    return result;
  }

  protected String createConfigId( final IFile file ) {
    String name = file.getName();
    // FIXME: Remove when Galileo is no longer supported.
    ILaunchManager mgr = getLaunchManager();
    return ILaunchManagerCompat.generateLaunchConfigurationName( mgr, name );
  }
}
