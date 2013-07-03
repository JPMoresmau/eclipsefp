package net.sf.eclipsefp.haskell.ui.actions;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalImplDetails;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalImplDetails.SandboxType;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.views.CabalPackagesView;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * <p>Cabal install action, contextual on projects</p>
  *
  * @author JP Moresmau
 */
public class CabalInstallAction implements IObjectActionDelegate {
  private final Set<IProject> projects=new LinkedHashSet<IProject>();
  private Shell currentShell;

  @Override
  public void setActivePart( final IAction arg0, final IWorkbenchPart arg1 ) {
    currentShell=arg1.getSite().getShell();

  }

  /**
   * command for cabal-dev invocation
   * @return
   */
  protected String getCabalDevCommand(){
    return "install";
  }

  /**
   * run cabal dev into given sandbox
   * @param sandbox
   */
  protected void runCabalDev(final String sandbox){
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    String cabalDev = preferenceStore.getString( IPreferenceConstants.CABALDEV_EXECUTABLE );
    if (cabalExecutable!=null){

      final List<String> commands = new ArrayList<String>();
      commands.add( cabalDev );
      commands.add( getCabalDevCommand());
   // options
      commands.add("--builddir="+BWFacade.DIST_FOLDER_CABAL);
      ScionManager.addCabalInstallOptions( commands );
      commands.add("--force-reinstalls");
      commands.add("--sandbox="+sandbox);
      commands.add("--with-cabal-install="+cabalExecutable);

      addExtraParameters(commands);
      for (final IProject p:projects){
        try {
          List<String> prjCommands = new ArrayList<String>(commands);
          BWFacade bf=BuildWrapperPlugin.getFacade( p );
          // need to provide user supplied info
          if(bf!=null){
            String f=bf.getFlags();
            if (f!=null && f.length()>0){
              prjCommands.add("--flags="+f);
            }
            List<String> extraOpts=bf.getExtraOpts();
            if (extraOpts!=null){
              for (String eo:extraOpts){
                prjCommands.add(eo);
              }
            }
          }


          AbstractHaskellLaunchDelegate.runInConsole(p, prjCommands, new File(p.getLocation().toOSString()), NLS.bind( getJobName(), p.getName() ),true,getAfter(p) );
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( currentShell, UITexts.install_error, UITexts.install_error_text, st);
        }


      }
    }
  }

  @Override
  public void run( final IAction arg0 ) {
    // this action and its subclasses use cabal executable directly
    // so ask for confirmation that we're getting out of the sandbox
    // (dependencies and installation in dependent projects being handled automatically)
    CabalImplDetails cid=ScionManager.getCabalImplDetails();
    if (cid.isSandboxed()){
      if (cid.getType().equals( SandboxType.CABAL_DEV )){
        MessageDialog md=new MessageDialog( currentShell, UITexts.install_sandbox_title, null, getSandboxWarningMessage(), MessageDialog.QUESTION,
            new String[] { IDialogConstants.OK_LABEL, IDialogConstants.CANCEL_LABEL, UITexts.install_sandbox_install_dest }, 2 );
        int ret=md.open();
        if (ret==1){
          return;
        } else if (ret==2){
          DirectoryDialog dd=new DirectoryDialog( currentShell );
          dd.setText( UITexts.install_sandbox_install_dest );
          String fileName=dd.open();
          if (fileName!=null){
            runCabalDev(fileName);
          }
          return;
        }
      } else {
        boolean confirm=MessageDialog.openConfirm( currentShell, UITexts.install_sandbox_title, getSandboxWarningMessage() );
        if (!confirm){
          return;
        }
      }
    }

    // do not ask for options, use our own dist folder and default cabal options
   //WizardDialog wd=new WizardDialog( currentShell, new CabalInstallWizard( projects ) );
   //wd.open();
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("install");
      ScionManager.addCabalInstallOptions( commands );
   // options
      commands.add("--builddir="+BWFacade.DIST_FOLDER_CABAL);
      // commands.add( "--user" ); // use cabal default, which is now user
      addExtraParameters(commands);
      for (final IProject p:projects){
        try {
          List<String> prjCommands = new ArrayList<String>(commands);
          BWFacade bf=BuildWrapperPlugin.getFacade( p );
          // need to provide user supplied info
          if(bf!=null){
            String f=bf.getFlags();
            if (f!=null && f.length()>0){
              prjCommands.add("--flags="+f);
            }
            List<String> extraOpts=bf.getExtraOpts();
            if (extraOpts!=null){
              for (String eo:extraOpts){
                prjCommands.add(eo);
              }
            }
          }


          AbstractHaskellLaunchDelegate.runInConsole(p, prjCommands, new File(p.getLocation().toOSString()), NLS.bind( getJobName(), p.getName() ),true,getAfter(p) );
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( currentShell, UITexts.install_error, UITexts.install_error_text, st);
        }


      }
    }


  }

  protected Runnable getAfter(final IProject p){
    return new Runnable() {

      @Override
      public void run() {
        /** refresh the cabal packages view **/
        CabalPackagesView.refresh();
      }
    };
  }

  protected String getJobName(){
    return UITexts.install_job;
  }

  protected String getSandboxWarningMessage(){
    return UITexts.install_sandbox_install_text;
  }

  protected void addExtraParameters(final List<String> commands){
    // force reinstall since we're probably reinstalling our development version
    commands.add( "--reinstall" );
  }

  @Override
  public void selectionChanged( final IAction arg0, final ISelection arg1 ) {
    projects.clear();
    projects.addAll( ResourceUtil.getProjects( arg1 ) );
  }

}
