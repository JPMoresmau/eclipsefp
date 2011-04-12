package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
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


public class CabalTestWizard extends Wizard {
  private final IProject project;
  private CabalTestOptionsPage optionsPage;

  public CabalTestWizard(final IProject project) {
    super();
    this.project=project;
    setWindowTitle( Platform.getResourceBundle( Platform.getBundle( HaskellUIPlugin.getPluginId() )).getString( "cabalTestWizard.name" ));
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
  }

  @Override
  public void addPages() {
    optionsPage=new CabalTestOptionsPage(project) ;
    addPage(optionsPage );
  }

  @Override
  public boolean performFinish() {
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("test");
   // options
      commands.add("--builddir="+optionsPage.getFolder());

      // test suites as extra arguments

        try {
          AbstractHaskellLaunchDelegate.runInConsole( commands, new File(project.getLocation().toOSString()), NLS.bind( UITexts.test_job, project.getName() ),true );
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( getShell(), UITexts.test_error, UITexts.test_error_text, st);
        }



    }
     return true;
  }

}
