package net.sf.eclipsefp.haskell.ui.web.wizards;

import java.net.URI;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;


public class NewSnapProjectWizard extends Wizard implements INewWizard {

  private WizardNewProjectCreationPage mainPage;

  public NewSnapProjectWizard() {
    super();
    setWindowTitle( UITexts.newSnapProjectWizard_windowTitle );
  }

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
    String name = mainPage.getProjectName();
    URI location = null;
    if( !mainPage.useDefaults() ) {
      location = mainPage.getLocationURI();
    } // else location == null
    IProject project = CustomProjectSupport.createBaseProject(name, location);
    // Then call "snap init" on the project
    try {
      String[] cmdLine = new String[] { "snap", "init" };
      IPath path = project.getLocation();
      Process p = Runtime.getRuntime().exec(cmdLine, null, path.toFile());
      // Parse the output
      p.waitFor();
      CustomProjectSupport.addNature(project, HaskellNature.NATURE_ID);
      project.refreshLocal( IResource.DEPTH_INFINITE, null );
    } catch (Exception e) {
      MessageDialog
      .openError(
          getShell(),
          "Snap could not be run",
          "Snap was not found in your system or returned an error. "
              + "You can install it running \"cabal install snap\" in a console." );
      return false;
    }
    return true;
  }

}
