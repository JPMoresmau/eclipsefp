package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;

/**
 * <p>Wizard to run cabal install on a project</p>
  *
  * @author JP Moresmau
 */
public class CabalInstallWizard extends Wizard {
  private final Set<IProject> projects;
  private CabalInstallOptionsPage optionsPage;

  public CabalInstallWizard(final Set<IProject> projects) {
    super();
    this.projects=projects;
    setWindowTitle( Platform.getResourceBundle( Platform.getBundle( HaskellUIPlugin.getPluginId() )).getString( "cabalInstallWizard.name" ));
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
  }

  @Override
  public void addPages() {
    optionsPage=new CabalInstallOptionsPage(projects) ;
    addPage(optionsPage );
  }

  @Override
  public boolean performFinish() {
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("install");
   // options
      commands.add("--builddir="+optionsPage.getFolder());
      if (optionsPage.isGlobal()){
        commands.add( "--global" );
      } else {
        commands.add( "--user" );
      }
      // force reinstall since we're probably reinstalling our development version
      commands.add( "--reinstall" );
      for (final IProject p:projects){
        try {
          AbstractHaskellLaunchDelegate.runInConsole( commands, new File(p.getLocation().toOSString()), NLS.bind( UITexts.install_job, p.getName() ),true );
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( getShell(), UITexts.install_error, UITexts.install_error_text, st);
        }

//        new Job(NLS.bind( UITexts.install_job, p.getName() )) {
//
//          @Override
//          protected IStatus run(final IProgressMonitor arg0) {
//            ProcessBuilder pb=new ProcessBuilder( commands );
//            pb.directory( new File(p.getLocation().toOSString()) );
//            pb.redirectErrorStream( true );
            /*IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
            IOConsole console = new IOConsole(NLS.bind( UITexts.install_job,p.getName()), null);

            mgr.addConsoles(new org.eclipse.ui.console.IConsole[] {console});
            mgr.showConsoleView( console );

            try {
              int ret=new ProcessRunner().executeBlocking( new File(p.getLocation().toOSString()) ,new OutputStreamWriter( console.newOutputStream()), null, commands.toArray( new String[commands.size()]));
              if (ret!=0){
                final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), "");
                Display.getDefault().asyncExec( new Runnable(){
                  public void run() {
                    ErrorDialog.openError( getShell(), UITexts.install_error, UITexts.install_error_text, st );
                  }
                } );
              }
              return Status.OK_STATUS;
            } catch (IOException ioe){
              HaskellUIPlugin.log(ioe);
             return  new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.install_error,ioe );
            }finally {
              arg0.done();
            }
            */
//            try {
//              ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
//              String configTypeId = HaskellLaunchDelegate.class.getName();
//              ILaunchConfigurationType configType  = launchManager.getLaunchConfigurationType( configTypeId );
//              ILaunchConfigurationWorkingCopy wc=configType.newInstance( null, UITexts.install_job );
//              wc.setAttribute( IDebugUIConstants.ATTR_PRIVATE, true );
//              wc.setAttribute( IDebugUIConstants.ATTR_CAPTURE_IN_CONSOLE, true );
//              ILaunch launch = new Launch(wc,ILaunchManager.RUN_MODE,null);
//
//              Process jp=pb.start();
//              IProcess ep = DebugPlugin.newProcess(launch, jp,UITexts.install_job);
//              ep.setAttribute( IProcess.ATTR_CMDLINE,
//                  CommandLineUtil.renderCommandLine( commands.toArray( new String[commands.size()] )) );
//              ep.setAttribute( IProcess.ATTR_PROCESS_TYPE, getClass().getName() );
//              launch.addProcess(ep);
//              launchManager.addLaunch(launch);
//
//            } catch (Exception ioe){
//              HaskellUIPlugin.log(ioe);
//            //  return  new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.install_error,ioe );
//            }
//          }
//        }.schedule();

      }
    }
     return true;

  }
}
