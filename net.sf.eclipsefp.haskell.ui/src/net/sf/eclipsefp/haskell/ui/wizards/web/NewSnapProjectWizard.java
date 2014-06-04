package net.sf.eclipsefp.haskell.ui.wizards.web;

import java.net.URI;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.ui.internal.backend.BackendManager;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;

/**
 *
 * @author Alejandro Serrano
 *
 */
public class NewSnapProjectWizard extends Wizard implements INewWizard {

  private WizardNewProjectCreationPage mainPage;

  public NewSnapProjectWizard() {
    super();
    setWindowTitle( UITexts.newSnapProjectWizard_windowTitle );
  }

  @Override
  public void init( final IWorkbench workbench,
      final IStructuredSelection selection ) {
    // Do nothing
  }

  @Override
  public void addPages() {
    super.addPages();
    mainPage = new WizardNewProjectCreationPage(
        UITexts.newSnapProjectWizard_pageTitle );
    mainPage.setTitle( UITexts.newSnapProjectWizard_pageTitle );
    mainPage.setDescription( UITexts.newSnapProjectWizard_pageDesc );
    addPage( mainPage );
  }

  @Override
  public boolean performFinish() {
    final String name = mainPage.getProjectName();
    final URI location =   ( !mainPage.useDefaults() )
      ?  mainPage.getLocationURI()
        :null;
     // else location == null
    new Job(UITexts.newYesodProjectWizard_job) {

      @Override
      protected IStatus run( final IProgressMonitor arg0 ) {
        try {
          IProject project = CustomProjectSupport.createBaseProject(name, location);
          String serverExecutable =BackendManager.getExecutablePath( IPreferenceConstants.SNAP_EXECUTABLE, "snap",false );

          String[] cmdLine = new String[] { serverExecutable, "init" };
          IPath path = project.getLocation();
          Process p = Runtime.getRuntime().exec(cmdLine, null, path.toFile());
          Thread[] ts=ProcessRunner.consume( p );
          // Parse the output
          p.waitFor();
          for (Thread t:ts){
            t.join();
          }
          CustomProjectSupport.addNature(project, HaskellNature.NATURE_ID);
          project.refreshLocal( IResource.DEPTH_INFINITE, null );
        } catch (Exception e) {
          HaskellCorePlugin.log(  UITexts.newSnapProjectWizard_error, e );
          Display.getDefault().asyncExec( new Runnable(){
            @Override
            public void run() {
              MessageDialog
              .openError(
                  getShell(),
                  UITexts.newSnapProjectWizard_error_title,
                  UITexts.newSnapProjectWizard_error_message);
            }
          } );


        }
        return Status.OK_STATUS;
      }
    }.schedule();
    return true;
  }

}
