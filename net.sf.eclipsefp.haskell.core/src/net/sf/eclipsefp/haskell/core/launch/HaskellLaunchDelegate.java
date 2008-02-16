// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.launch;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.debug.core.model.IProcess;



/** <p>Implements the launching functionality for Haskell launch
  * configurations.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellLaunchDelegate implements ILaunchConfigurationDelegate {

  public void launch( final ILaunchConfiguration configuration,
                      final String mode,
                      final ILaunch launch,
                      final IProgressMonitor monitor ) throws CoreException {
    if( !monitor.isCanceled() ) {
      try {
        IPath loc = getLocation( configuration );
        checkCancellation( monitor );
        String[] arguments = determineArguments( configuration );
        checkCancellation( monitor );
        String[] cmdLine = createCmdLine( loc, arguments );
        checkCancellation( monitor );
        File workingDir = determineWorkingDir( configuration );
        checkCancellation( monitor );
        IProcess process = createProcess( launch, loc, cmdLine, workingDir );
        if( !isBackground( configuration ) ) {
          while( !process.isTerminated() ) {
            try {
              if( monitor.isCanceled() ) {
                process.terminate();
                break;
              }
              Thread.sleep( 50 );
            } catch( InterruptedException iex ) {
              // ignored
            }
          }
        }
      } catch( LaunchCancelledException lcex ) {
        // cancelled on user request
      }
    }
  }

  private IProcess createProcess( final ILaunch launch,
                                  final IPath location,
                                  final String[] cmdLine,
                                  final File workingDir ) throws CoreException {
    Process proc = DebugPlugin.exec( cmdLine, workingDir );
    Map<String, String> processAttrs = new HashMap<String, String>();
    String programName = determineProgramName( location );
    processAttrs.put( IProcess.ATTR_PROCESS_TYPE, programName );
    IProcess process = null;
    if( proc != null ) {
      String loc = location.toOSString();
      process = DebugPlugin.newProcess( launch, proc, loc, processAttrs );
      process.setAttribute( IProcess.ATTR_CMDLINE,
                            CommandLineUtil.renderCommandLine( cmdLine ) );
    }
    return process;
  }

  private String[] createCmdLine( final IPath location,
                                  final String[] arguments ) {
    int cmdLineLength = 1;
    if( arguments != null ) {
      cmdLineLength += arguments.length;
    }
    String[] cmdLine = new String[ cmdLineLength ];
    cmdLine[ 0 ] = location.toOSString();
    if( arguments != null ) {
      System.arraycopy( arguments, 0, cmdLine, 1, arguments.length );
    }
    return cmdLine;
  }

  private File determineWorkingDir( final ILaunchConfiguration config )
                                                          throws CoreException {
    String name = ILaunchAttributes.WORKING_DIRECTORY;
    String attribute = config.getAttribute( name, ( String )null );
    File result = null;
    if( attribute != null ) {
      result = new Path( attribute ).toFile();
    }
    return result;
  }

  private String[] determineArguments( final ILaunchConfiguration config )
                                                          throws CoreException {
    String args = config.getAttribute( ILaunchAttributes.ARGUMENTS,
                                       ( String )null );
    return CommandLineUtil.parse( args );
  }

  private void checkCancellation( final IProgressMonitor monitor ) {
    if( monitor.isCanceled() ) {
      throw new LaunchCancelledException();
    }
  }

  private String determineProgramName( final IPath location ) {
    String programName = location.lastSegment();
    String extension = location.getFileExtension();
    if( extension != null ) {
      int len = programName.length() - ( extension.length() + 1 );
      programName = programName.substring( 0, len );
    }
    return programName.toLowerCase();
  }


  // helping methods
  //////////////////

  public IPath getLocation( final ILaunchConfiguration config )
                                                          throws CoreException {
    String defaultValue = null;
    String location = config.getAttribute( ILaunchAttributes.EXECUTABLE,
                                           defaultValue );
    if( isEmpty( location ) ) {
      String msg = CoreTexts.haskellLaunchDelegate_noExe;
      String pluginId = HaskellCorePlugin.getPluginId();
      IStatus status = new Status( IStatus.ERROR, pluginId, 0, msg, null );
      throw new CoreException( status );
    }
    return new Path( location );
  }

  private boolean isEmpty( final String location ) {
    return    location == null
           || location.trim().length() == 0;
  }

  private boolean isBackground( final ILaunchConfiguration config )
                                                          throws CoreException {
    return config.getAttribute( ILaunchAttributes.RUN_IN_BACKGROUND, true );
  }

  private class LaunchCancelledException extends RuntimeException {
    private static final long serialVersionUID = 1912643423745032866L;

    private LaunchCancelledException() {
      super();
    }
  }
}