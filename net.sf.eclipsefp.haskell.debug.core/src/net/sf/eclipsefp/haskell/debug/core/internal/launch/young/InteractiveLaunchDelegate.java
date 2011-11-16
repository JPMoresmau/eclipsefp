/**
 *
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch.young;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.debug.HaskellDebugTarget;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.CommandOnChangeListener;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.core.model.IProcess;


/**
 * @author Alejandro Serrano
 *
 */
public class InteractiveLaunchDelegate extends AbstractHaskellLaunchDelegate {

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #postProcessCreation(org.eclipse.debug.core.ILaunchConfiguration,
   * java.lang.String, org.eclipse.debug.core.ILaunch,
   * org.eclipse.debug.core.model.IProcess)
   */
  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process )
      throws CoreException {
    if( mode.equals( ILaunchManager.DEBUG_MODE ) ) {
      HaskellDebugTarget hdt = new HaskellDebugTarget( launch, process );
      launch.addDebugTarget( hdt );
      hdt.start();
    }

    registerReloadListener( configuration, launch, process );
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #preProcessCreation(org.eclipse.debug.core.ILaunchConfiguration,
   * java.lang.String, org.eclipse.debug.core.ILaunch, java.util.Map)
   */
  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch,
      final Map<String, String> processAttribs ) {
    // NOOP
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #preProcessDefinitionCreation(org.eclipse.debug.core.ILaunchConfiguration,
   * java.lang.String, org.eclipse.debug.core.ILaunch)
   */
  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode,
      final ILaunch launch ) {
    // NOOP
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate#postProcessFinished()
   */
  @Override
  protected void postProcessFinished() {
    // NOOP
  }

  IInteractiveLaunchOperationDelegate getDelegate(
      final ILaunchConfiguration config ) throws CoreException {
    try {
      final ClassLoader cl = getClass().getClassLoader();
      String delegateClass = config.getAttribute(
          ILaunchAttributes.INTERPRETER, ILaunchAttributes.EMPTY );
      IInteractiveLaunchOperationDelegate delegate = ( IInteractiveLaunchOperationDelegate )cl
          .loadClass( delegateClass ).newInstance();
      return delegate;
    } catch( Exception e ) {
      throw new CoreException( Status.CANCEL_STATUS );
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #getExecutableLocation(org.eclipse.debug.core.ILaunchConfiguration)
   */
  @Override
  public IPath getExecutableLocation( final ILaunchConfiguration config )
      throws CoreException {
    IInteractiveLaunchOperationDelegate delegate = getDelegate( config );
    String location = delegate.getExecutable();
    return new Path( location );
  }

  /*
   * (non-Javadoc)
   *
   * @see net.sf.eclipsefp.haskell.debug.core.internal.launch.young.
   * AbstractHaskellLaunchDelegate
   * #getUserArguments(org.eclipse.debug.core.ILaunchConfiguration)
   */
  @Override
  protected String getUserArguments( final ILaunchConfiguration config )
      throws CoreException {
    IInteractiveLaunchOperationDelegate delegate = getDelegate( config );
    String projectName = config.getAttribute( ILaunchAttributes.PROJECT_NAME,
        ILaunchAttributes.EMPTY );
    List<String> files = config.getAttribute(
        ILaunchAttributes.FILES, new ArrayList<String>() );
    IProject project = ResourcesPlugin.getWorkspace().getRoot()
        .getProject( projectName );
    // Get IFiles from filenames
    IFile[] realFiles = new IFile[ files.size() ];
    for( int i = 0; i < files.size(); i++ ) {
      String file = files.get( i );
      IFile realFile = project.getFile( file );
      realFiles[ i ] = realFile;
    }
    StringBuilder sb = new StringBuilder();
    for( String arg: delegate.createArguments( project, realFiles ) ) {
      sb.append( arg );
      sb.append( ' ' );
    }
    return sb.toString() + super.getUserArguments( config );
  }

  public static void commandToProcess( final IProcess p, final String command )
      throws CoreException {
    try {
      if( command != null && command.length() > 0 ) {
        p.getStreamsProxy().write( command );
        p.getStreamsProxy().write( PlatformUtil.NL );
      }
    } catch( IOException ioe ) {
      Status status = new Status( IStatus.ERROR,
          HaskellDebugCore.getPluginId(), CoreTexts.console_command_failed, ioe );
      throw new CoreException( status );
    }
  }

  private void registerReloadListener(
      final ILaunchConfiguration configuration, final ILaunch launch,
      final IProcess process ) throws CoreException {
    final String command = configuration.getAttribute(
        ILaunchAttributes.COMMAND, ( String )null );
    commandToProcess( process, command );

    final String reloadCommand = configuration.getAttribute(
        ILaunchAttributes.RELOAD_COMMAND, ( String )null );
    final String project = configuration.getAttribute(
        ILaunchAttributes.PROJECT_NAME, ( String )null );

    if( reloadCommand != null
        && configuration.getAttribute( ILaunchAttributes.RELOAD, false ) ) {

      final boolean commandOnReload = configuration.getAttribute(
          ILaunchAttributes.COMMAND_ON_RELOAD, false );

      final CommandOnChangeListener cocl = new CommandOnChangeListener(
          process, project, reloadCommand, commandOnReload ? command : null );

      ResourcesPlugin.getWorkspace().addResourceChangeListener( cocl,
          IResourceChangeEvent.PRE_BUILD );

      ILaunchesListener2 ll = new ILaunchesListener2() {

        public void launchesRemoved( final ILaunch[] launches ) {
          // NOOP

        }

        public void launchesChanged( final ILaunch[] launches ) {
          // NOOP

        }

        public void launchesAdded( final ILaunch[] launches ) {
          // NOOP

        }

        public void launchesTerminated( final ILaunch[] launches ) {
          for( ILaunch l: launches ) {
            if( launch.equals( l ) ) {
              ResourcesPlugin.getWorkspace()
                  .removeResourceChangeListener( cocl );
              DebugPlugin.getDefault().getLaunchManager()
                  .removeLaunchListener( this );
            }
          }
        }
      };
      DebugPlugin.getDefault().getLaunchManager().addLaunchListener( ll );
    }
  }

}
