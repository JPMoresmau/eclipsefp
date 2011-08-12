package net.sf.eclipsefp.haskell.ui.web.wizards;

import java.io.OutputStreamWriter;
import java.net.URI;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;


public class NewYesodProjectWizard extends Wizard implements INewWizard {

  private WizardNewProjectCreationPage mainPage;

  public NewYesodProjectWizard() {
    super();
    setWindowTitle( UITexts.newYesodProjectWizard_windowTitle );
  }

  public void init( final IWorkbench workbench,
      final IStructuredSelection selection ) {
    // Do nothing
  }

  @Override
  public void addPages() {
    super.addPages();
    mainPage = new WizardNewProjectCreationPage(
        UITexts.newYesodProjectWizard_pageTitle );
    mainPage.setTitle( UITexts.newYesodProjectWizard_pageTitle );
    mainPage.setDescription( UITexts.newYesodProjectWizard_pageDesc );
    addPage( mainPage );
  }

  @Override
  public boolean performFinish() {
    String name = mainPage.getProjectName();
    URI location = null;
    if( !mainPage.useDefaults() ) {
      location = mainPage.getLocationURI();
    } // else location == null

    try {
      // Get parent path
      IPath parentPath = mainPage.getLocationPath();
      // IPath parentPath = path.removeLastSegments( 1 );
      // Run "yesod init"
      String[] cmdLine = new String[] { "yesod", "init" };
      Process p = Runtime.getRuntime()
          .exec( cmdLine, null, parentPath.toFile() );
      // Get the things to write
      OutputStreamWriter inS = new OutputStreamWriter( p.getOutputStream() );
      inS.write( "user\n");
      inS.write( name + "\n" );
      inS.write( "Foundation\n" );
      inS.write( "s\n" );
      p.waitFor();

      IProject project = CustomProjectSupport
          .createBaseProject( name, location );
      CustomProjectSupport.addNature( project, HaskellNature.NATURE_ID );
      project.refreshLocal( IResource.DEPTH_INFINITE, null );
    } catch( Exception e ) {
      // Add error message when "snap" is not installed
      return false;
    }
    return true;
  }

}
