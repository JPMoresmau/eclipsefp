/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.util.CommandLineUtil;
import net.sf.eclipsefp.haskell.util.NetworkUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;

/**
 * abstract base class for our launch delegates
 * @author JP Moresmau
 *
 */
public abstract class AbstractHaskellLaunchDelegate extends LaunchConfigurationDelegate{

  public static IInteractiveLaunchOperationDelegate getDelegate(final ILaunchConfiguration configuration) throws CoreException{
    String delegateClass=configuration.getAttribute( ILaunchAttributes.DELEGATE, "" ); //$NON-NLS-1$
    IInteractiveLaunchOperationDelegate delegate=null;
    if (delegateClass.length()>0){
      try {
        delegate = InteractiveDelegateManager.getContributors().get( delegateClass );
      } catch (Throwable e){
        HaskellDebugCore.log( e.getLocalizedMessage(), e );
      }
    }
    return delegate;
  }

  @Override
  public void launch( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, IProgressMonitor monitor )
      throws CoreException {
    if (monitor==null){
      monitor=new NullProgressMonitor();
    }
    if( !monitor.isCanceled() ) {
      try {
        IInteractiveLaunchOperationDelegate delegate=getDelegate( configuration );
        monitor.beginTask( configuration.getName(), 3 );
        final IPath loc =delegate!=null?new Path(delegate.getExecutable()) :
          getExecutableLocation( configuration );
        checkCancellation( monitor );
        String[] arguments = determineArguments( configuration,delegate,mode );
        checkCancellation( monitor );
        String[] cmdLine = createCmdLine( loc, arguments );
        checkCancellation( monitor );
        File workingDir = determineWorkingDir( configuration );
        checkCancellation( monitor );
        monitor.worked( 1 );
        IProcess process = createProcess( configuration, mode, launch, loc,
            cmdLine, workingDir );
        monitor.worked( 1 );
        if( process != null ) {
          postProcessCreation( configuration, mode, launch, process );
        }
        monitor.done();
        /*
         * DebugPlugin.getDefault().getLaunchManager().addLaunchListener( new
         * ILaunchesListener() {
         *
         * public void launchesRemoved( final ILaunch[] launches ) { for
         * (ILaunch l:launches){ System.out.println("removed:" +l.toString()); }
         *
         * }
         *
         * public void launchesChanged( final ILaunch[] launches ) { for
         * (ILaunch l:launches){ System.out.println("changed:" +l.toString()); }
         *
         * }
         *
         * public void launchesAdded( final ILaunch[] launches ) { for (ILaunch
         * l:launches){ System.out.println("added:" +l.toString()); }
         *
         * } });
         */

        if( process != null ) {
          if ( isBackground( configuration ) || Display.findDisplay( Thread.currentThread())!=null) {
            final IProcess theProcess = process;
            Job endJob = new Job( NLS.bind( CoreTexts.running , loc.toOSString() )) {

              @Override
              protected IStatus run( final IProgressMonitor mon ) {
                try {
                  while( !theProcess.isTerminated() ) {
                    try {
                      if( mon.isCanceled()) {
                        theProcess.terminate();
                        return Status.CANCEL_STATUS;
                      }
                      Thread.sleep( 50 );
                    } catch( InterruptedException iex ) {
                      // ignored
                    } catch ( DebugException ex ) {
                      // ignored
                    }
                  }
                } finally {
                  try {
                    postProcessFinished(configuration);
                  } catch(CoreException ce){
                    HaskellCorePlugin.log( ce );
                  }
                }
                return Status.OK_STATUS;
              }
            };
            endJob.schedule();
          } else {
            while( !process.isTerminated() ) {
              try {
                if(monitor!=null && monitor.isCanceled() ) {
                  process.terminate();
                  break;
                }
                Thread.sleep( 50 );
              } catch( InterruptedException iex ) {
                // ignored
              }
            }
            postProcessFinished(configuration);
          }
        }
      } catch( LaunchCancelledException lcex ) {
        // canceled on user request
      }
    }
  }

  protected abstract void postProcessCreation(final ILaunchConfiguration configuration,
      final String mode,final ILaunch launch,IProcess process) throws CoreException;
  protected abstract void preProcessCreation(final ILaunchConfiguration configuration,
      final String mode,final ILaunch launch,Map<String, String> processAttribs) throws CoreException;
  protected abstract void preProcessDefinitionCreation(final ILaunchConfiguration configuration,
      final String mode,final ILaunch launch) throws CoreException;
  protected abstract void postProcessFinished(final ILaunchConfiguration configuration) throws CoreException;

  private IProcess createProcess( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IPath location,
      final String[] cmdLine, final File workingDir ) throws CoreException {
    // Process proc = DebugPlugin.exec( cmdLine, workingDir );
    ProcessBuilder pb = new ProcessBuilder( cmdLine );
    pb.directory( workingDir );
    if( configuration.getAttribute( ILaunchAttributes.SYNC_STREAMS, true ) ) {
      pb.redirectErrorStream( true );
    }
    if (configuration.getAttribute( ILaunchAttributes.NEEDS_HTTP_PROXY, false ) ) {
      NetworkUtil.addHTTP_PROXY_env( pb,NetworkUtil.HACKAGE_URL  );
    }
    try {
      preProcessDefinitionCreation( configuration, mode, launch );

      Map<String, String> processAttrs = new HashMap<String, String>();
      String programName = determineProgramName( location );
      processAttrs.put( IProcess.ATTR_PROCESS_TYPE, programName );
      preProcessCreation( configuration, mode, launch, processAttrs );
      IProcess process = null;
      Process proc = pb.start();
      if( proc != null ) {
        //String loc = location.toOSString();
//        process = new RuntimeProcess( launch, proc, configuration.getName(), processAttrs){
//          @Override
//          protected org.eclipse.debug.core.model.IStreamsProxy createStreamsProxy() {
//
//              String encoding = getLaunch().getAttribute(DebugPlugin.ATTR_CONSOLE_ENCODING);
//              return new DelayedStreamsProxy(getSystemProcess(), encoding);
//          }
//        };
          process=DebugPlugin.newProcess( launch, proc, configuration.getName(), processAttrs );
          process.setAttribute( IProcess.ATTR_CMDLINE, CommandLineUtil
                .renderCommandLine( cmdLine ) );
      }
      return process;
    } catch( IOException e ) {
      Status status = new Status( IStatus.ERROR,
          HaskellDebugCore.getPluginId(),
          CoreTexts.haskellLaunchDelegate_noProcess, e );
      throw new CoreException( status );
    }

  }

  private String[] createCmdLine(final IPath location, final String[] arguments ) {
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

  protected File determineWorkingDir( final ILaunchConfiguration config )
      throws CoreException {
    String name = ILaunchAttributes.WORKING_DIRECTORY;
    String attribute = config.getAttribute( name, ( String )null );
    File result = null;
    if( attribute != null ) {
      result = new Path( attribute ).toFile();
    }
    return result;
  }

  String[] determineArguments( final ILaunchConfiguration config,final IInteractiveLaunchOperationDelegate delegate,final String mode )
      throws CoreException {
    String extra = config.getAttribute( ILaunchAttributes.EXTRA_ARGUMENTS,
        ILaunchAttributes.EMPTY );
    String args = config.getAttribute( ILaunchAttributes.ARGUMENTS,
        ILaunchAttributes.EMPTY );
    String[] fullArgs=CommandLineUtil.parse( extra + " " + args ); //$NON-NLS-1$

    String[] delegateArgs=getDelegateArguments(config,delegate,mode);
    if (delegateArgs.length>0){
      String[] newArgs=new String[fullArgs.length+delegateArgs.length];
      System.arraycopy( fullArgs, 0, newArgs, 0, fullArgs.length );
      System.arraycopy( delegateArgs, 0, newArgs, fullArgs.length, delegateArgs.length );
      fullArgs=newArgs;
    }

    return fullArgs;
  }

  public static String[] getDelegateArguments(final ILaunchConfiguration config,final IInteractiveLaunchOperationDelegate delegate,final String mode)throws CoreException{
    if (delegate!=null){
      IProject p=getProject( config );
      List<String> fileNames=config.getAttribute( ILaunchAttributes.FILES, new ArrayList<String>() );
      IFile[] files=new IFile[fileNames.size()];
      for (int a=0;a<fileNames.size();a++){
        files[a]=p.getFile( fileNames.get(a) );
      }
      return delegate.createArguments(p , files,mode );
    }
    return new String[0];
  }

  private void checkCancellation( final IProgressMonitor monitor ) {
    if(monitor!=null && monitor.isCanceled() ) {
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
  // ////////////////

  public IPath getExecutableLocation( final ILaunchConfiguration config )
      throws CoreException {
    String location = config.getAttribute( ILaunchAttributes.EXECUTABLE,
        (String)null );
    if( isEmpty( location ) ) {
      String stanza = config.getAttribute( ILaunchAttributes.STANZA,
          (String)null );
      if (isEmpty( stanza )){
        String msg = CoreTexts.haskellLaunchDelegate_noExe;
        String pluginId = HaskellDebugCore.getPluginId();
        IStatus status = new Status( IStatus.ERROR, pluginId, 0, msg, null );
        throw new CoreException( status );
      }
      return ResourceUtil.getExecutableLocation( getProject( config ), stanza ).getLocation();

    }

    return new Path( location );
  }

  public static IProject getProject(final ILaunchConfiguration config)throws CoreException{
    String prj=config.getAttribute( ILaunchAttributes.PROJECT_NAME, (String)null );
    if (prj!=null){
     IProject p=ResourcesPlugin.getWorkspace().getRoot().getProject( prj );
     if (p!=null){
       return p;
     }
    }
    return null;
  }

  protected boolean isEmpty( final String location ) {
    return location == null || location.trim().length() == 0;
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

  public static void runInConsole(final IProject prj,final List<String> commands,final File directory,final String title,final boolean needsHTTP_PROXY) throws CoreException{
    runInConsole( prj, commands, directory, title, needsHTTP_PROXY,null );
  }

  public static void runInConsole(final IProject prj,final List<String> commands,final File directory,final String title,final boolean needsHTTP_PROXY,final Runnable after) throws CoreException{

    final ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
    String configTypeId = ExecutableHaskellLaunchDelegate.class.getName();
    ILaunchConfigurationType configType  = launchManager.getLaunchConfigurationType( configTypeId );
    final ILaunchConfigurationWorkingCopy wc=configType.newInstance( null,launchManager.generateLaunchConfigurationName(title) );//launchManager.generateUniqueLaunchConfigurationNameFrom( title));

    // if private, we don't get the save dialog for unsaved files in project
    wc.setAttribute( IDebugUIConstants.ATTR_PRIVATE, prj==null );
    wc.setAttribute( IDebugUIConstants.ATTR_LAUNCH_IN_BACKGROUND, false );
    wc.setAttribute( IDebugUIConstants.ATTR_CAPTURE_IN_CONSOLE, true );

    if (prj!=null){
      wc.setAttribute(ILaunchAttributes.PROJECT_NAME,prj.getName());
    }

    wc.setAttribute(ILaunchAttributes.EXECUTABLE,commands.get( 0 ));
    if (directory!=null){
      wc.setAttribute(ILaunchAttributes.WORKING_DIRECTORY,directory.getAbsolutePath());
    }
    if (commands.size()>1){
      wc.setAttribute(ILaunchAttributes.EXTRA_ARGUMENTS,CommandLineUtil.renderCommandLine( commands.subList( 1, commands.size() ) ));
    }
    wc.setAttribute( ILaunchAttributes.NEEDS_HTTP_PROXY, needsHTTP_PROXY );

    if (after!=null){
      ILaunchesListener2 l=new ILaunchesListener2() {

        @Override
        public void launchesRemoved( final ILaunch[] paramArrayOfILaunch ) {
          // NOOP

        }

        @Override
        public void launchesChanged( final ILaunch[] paramArrayOfILaunch ) {
          // NOOP

        }

        @Override
        public void launchesAdded( final ILaunch[] paramArrayOfILaunch ) {
          // NOOP

        }

        @Override
        public void launchesTerminated( final ILaunch[] paramArrayOfILaunch ) {
         for (ILaunch la:paramArrayOfILaunch){
           if (la.getLaunchConfiguration()==wc){
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
