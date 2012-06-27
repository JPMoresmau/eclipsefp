// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.IOException;
import java.util.Map;
import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import net.sf.eclipsefp.haskell.debug.core.internal.debug.HaskellDebugTarget;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.core.model.IProcess;



/** <p>Implements the launching functionality for Haskell launch interactive (GHCi, Hugs)
  * configurations.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellLaunchDelegate extends AbstractHaskellLaunchDelegate {


  // static final String DEBUG_PROCESS_TYPE = "net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci.GhciProcessType"; //$NON-NLS-1$

  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final Map<String, String> processAttribs ) {
    // NOOP
  }

  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process ) throws CoreException{

    process.setAttribute( HaskellDebugCore.PROCESS_COMMAND_HISTORY, Boolean.TRUE.toString() );

    if (mode.equals( ILaunchManager.DEBUG_MODE )){
      HaskellDebugTarget hdt=new HaskellDebugTarget( launch, process );
      launch.addDebugTarget(hdt);
      hdt.start();
    }

    registerReloadListener(configuration,launch,process);

  }

  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch ) {
    // NOOP

  }

  @Override
  protected void postProcessFinished(final ILaunchConfiguration configuration) {
    // NOOP
  }

  public static void commandToProcess(final IProcess p,final String command) throws CoreException{
    try {
      if (command!=null && command.length()>0){
        p.getStreamsProxy().write( command );
        p.getStreamsProxy().write( PlatformUtil.NL );
      }
    } catch (IOException ioe){
      Status status = new Status(IStatus.ERROR, HaskellDebugCore.getPluginId(),CoreTexts.console_command_failed, ioe);
      throw new CoreException(status);
    }
  }

  private void registerReloadListener(final ILaunchConfiguration configuration,final ILaunch launch,final IProcess process) throws CoreException{
    final String command= configuration.getAttribute( ILaunchAttributes.COMMAND, (String)null );
    commandToProcess( process, command );

    String reloadCommand=configuration.getAttribute( ILaunchAttributes.RELOAD_COMMAND, (String)null );
    IInteractiveLaunchOperationDelegate del=getDelegate( configuration );
    if (del!=null && del.getReloadCommand()!=null){
      reloadCommand=del.getReloadCommand();
    }
    final String project=configuration.getAttribute( ILaunchAttributes.PROJECT_NAME, (String)null );

    if (reloadCommand!=null && configuration.getAttribute( ILaunchAttributes.RELOAD, false )){

      final boolean commandOnReload= configuration.getAttribute( ILaunchAttributes.COMMAND_ON_RELOAD, false );

      final CommandOnChangeListener cocl=new CommandOnChangeListener(process,project,
          reloadCommand,
          commandOnReload?command:null);

      ResourcesPlugin.getWorkspace().addResourceChangeListener(
         cocl, IResourceChangeEvent.PRE_BUILD );

      ILaunchesListener2 ll=new ILaunchesListener2() {

        @Override
        public void launchesRemoved( final ILaunch[] launches ) {
          // NOOP

        }

        @Override
        public void launchesChanged( final ILaunch[] launches ) {
          // NOOP

        }

        @Override
        public void launchesAdded( final ILaunch[] launches ) {
          // NOOP

        }

        @Override
        public void launchesTerminated( final ILaunch[] launches ) {
          for (ILaunch l:launches){
            if (launch.equals( l )){
              ResourcesPlugin.getWorkspace().removeResourceChangeListener( cocl );
              DebugPlugin.getDefault().getLaunchManager().removeLaunchListener( this );
            }
          }
        }
      };
      DebugPlugin.getDefault().getLaunchManager().addLaunchListener( ll );
    }
  }

}