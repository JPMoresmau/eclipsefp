package net.sf.eclipsefp.haskell.ui.web.wizards;

import java.io.OutputStreamWriter;
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


public class NewYesodProjectWizard extends Wizard implements INewWizard {

  private NewYesodProjectPage mainPage;

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
    mainPage = new NewYesodProjectPage( UITexts.newYesodProjectWizard_pageTitle );
    mainPage.setTitle( UITexts.newYesodProjectWizard_pageTitle );
    mainPage.setDescription( UITexts.newYesodProjectWizard_pageDesc );
    addPage( mainPage );
  }

  @Override
  public boolean performFinish() {
    if( !mainPage.isPageComplete() ) {
      return false;
    }

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
      inS.write( mainPage.getAuthor() + "\n" );
      inS.flush();
      inS.write( name + "\n" );
      inS.flush();
      inS.write( mainPage.getFoundation() + "\n" );
      inS.flush();
      inS.write( mainPage.getDatabase() + "\n" );
      inS.flush();
      p.waitFor();

      IProject project = CustomProjectSupport
          .createBaseProject( name, location );
      CustomProjectSupport.addNature( project, HaskellNature.NATURE_ID );
      project.refreshLocal( IResource.DEPTH_INFINITE, null );
    } catch( Exception e ) {
      MessageDialog
          .openError(
              getShell(),
              "Yesod could not be run",
              "Yesod was not found in your system or returned an error. "
                  + "You can install it running \"cabal install yesod\" in a console." );
      return false;
    }
    return true;
  }

}
