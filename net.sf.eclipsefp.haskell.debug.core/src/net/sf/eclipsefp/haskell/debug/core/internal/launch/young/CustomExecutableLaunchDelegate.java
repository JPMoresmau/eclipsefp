/**
 *
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch.young;

import java.io.File;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.CommandLineUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;


/**
 * @author Alejandro Serrano
 *
 */
public class CustomExecutableLaunchDelegate extends
    AbstractHaskellLaunchDelegate {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.AbstractHaskellLaunchDelegate#postProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.debug.core.model.IProcess)
   */
  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process ) {
    // NOOP
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.AbstractHaskellLaunchDelegate#preProcessCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, java.util.Map)
   */
  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final Map<String, String> processAttribs ) {
    // NOOP
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.AbstractHaskellLaunchDelegate#preProcessDefinitionCreation(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch)
   */
  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch ) {
    // NOOP
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.AbstractHaskellLaunchDelegate#postProcessFinished()
   */
  @Override
  protected void postProcessFinished() {
    // NOOP
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.AbstractHaskellLaunchDelegate#getExecutableLocation(org.eclipse.debug.core.ILaunchConfiguration)
   */
  @Override
  public IPath getExecutableLocation( final ILaunchConfiguration config )
      throws CoreException {
    String location = config.getAttribute( ILaunchAttributes.CUSTOM_EXECUTABLE,
        ILaunchAttributes.EMPTY );
    return new Path( location );
  }

  public static void runInConsole( final IProject prj,
      final List<String> commands, final File directory, final String title,
      final boolean needsHTTP_PROXY ) throws CoreException {
    runInConsole( prj, commands, directory, title, needsHTTP_PROXY, null );
  }

  public static void runInConsole( final IProject prj,
      final List<String> commands, final File directory, final String title,
      final boolean needsHTTP_PROXY, final Runnable after )
      throws CoreException {

    final ILaunchManager launchManager = DebugPlugin.getDefault()
        .getLaunchManager();
    String configTypeId = CustomExecutableLaunchDelegate.class.getName();
    ILaunchConfigurationType configType = launchManager
        .getLaunchConfigurationType( configTypeId );
    final ILaunchConfigurationWorkingCopy wc = configType.newInstance( null,
        title );// launchManager.generateUniqueLaunchConfigurationNameFrom(
                // title));

    // if private, we don't get the save dialog for unsave files in project
    wc.setAttribute( IDebugUIConstants.ATTR_PRIVATE, prj == null );
    wc.setAttribute( IDebugUIConstants.ATTR_LAUNCH_IN_BACKGROUND, false );
    wc.setAttribute( IDebugUIConstants.ATTR_CAPTURE_IN_CONSOLE, true );

    if( prj != null ) {
      wc.setAttribute( ILaunchAttributes.PROJECT_NAME, prj.getName() );
    }

    wc.setAttribute( ILaunchAttributes.CUSTOM_EXECUTABLE, commands.get( 0 ) );
    if( directory != null ) {
      wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY,
          directory.getAbsolutePath() );
    }
    if( commands.size() > 1 ) {
      wc.setAttribute(
          ILaunchAttributes.EXTRA_ARGUMENTS,
          CommandLineUtil.renderCommandLine( commands.subList( 1,
              commands.size() ) ) );
    }
    wc.setAttribute( ILaunchAttributes.NEEDS_HTTP_PROXY, needsHTTP_PROXY );

    if( after != null ) {
      ILaunchesListener2 l = new ILaunchesListener2() {

        public void launchesRemoved( final ILaunch[] paramArrayOfILaunch ) {
          // NOOP
        }

        public void launchesChanged( final ILaunch[] paramArrayOfILaunch ) {
          // NOOP
        }

        public void launchesAdded( final ILaunch[] paramArrayOfILaunch ) {
          // NOOP
        }

        public void launchesTerminated( final ILaunch[] paramArrayOfILaunch ) {
          for( ILaunch la: paramArrayOfILaunch ) {
            if( la.getLaunchConfiguration() == wc ) {
              after.run();
              launchManager.removeLaunchListener( this );
            }
          }
        }
      };
      launchManager.addLaunchListener( l );
    }

    // makes the console opens consistently
    DebugUITools.launch( wc, ILaunchManager.RUN_MODE );

  }
}
