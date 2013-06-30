package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
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
      ScionManager.addCabalInstallOptions( commands );
      // force reinstall since we're probably reinstalling our development version
      commands.add( "--reinstall" );
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


          AbstractHaskellLaunchDelegate.runInConsole(p, prjCommands, new File(p.getLocation().toOSString()), NLS.bind( UITexts.install_job, p.getName() ),true );
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( getShell(), UITexts.install_error, UITexts.install_error_text, st);
        }


      }
    }
     return true;

  }
}
