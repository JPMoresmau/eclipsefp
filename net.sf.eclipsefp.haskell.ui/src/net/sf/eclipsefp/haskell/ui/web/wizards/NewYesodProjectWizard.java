package net.sf.eclipsefp.haskell.ui.web.wizards;

import java.io.File;
import java.io.OutputStreamWriter;
import java.net.URI;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyOperation;

/**
*
* @author Alejandro Serrano
*
*/
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

    final String name = mainPage.getProjectName();
    final URI location =   ( !mainPage.useDefaults() )
        ?  mainPage.getLocationURI()
          :null;

     final String author=   mainPage.getAuthor();
     final String foundation= mainPage.getFoundation();
     final char database= mainPage.getDatabase();
        // IPath parentPath = path.removeLastSegments( 1 );
        // Run "yesod init"
      // Get parent path
      final IPath parentPath = mainPage.getLocationPath();
      new Job(UITexts.newYesodProjectWizard_job) {

        @Override
        protected IStatus run( final IProgressMonitor arg0 ) {
          try {
            File f=FileUtil.findExecutableInPath( "yesod" );

            String[] cmdLine = new String[] {f!=null?f.getAbsolutePath():"yesod", "init"};
            //new ProcessRunner().executeBlocking( parentPath.toFile(), new StringWriter(), new StringWriter(), "yesod", "init" );
            Process p = Runtime.getRuntime()
                .exec( cmdLine, null, parentPath.toFile() );
            Thread[] ts=ProcessRunner.consume( p );

            // Get the things to write
            OutputStreamWriter inS = new OutputStreamWriter( p.getOutputStream() );
            inS.write(author  + "\n" );
            inS.flush();
            inS.write( name + "\n" );
            inS.flush();
            inS.write( "\n" ); // directory empty use project name
            inS.flush();
            inS.write(foundation + "\n" );
            inS.flush();
            inS.write(database  + "\n" );
            inS.flush();
            p.waitFor();
            for (Thread t:ts){
              t.join();
            }
            IProgressMonitor mon = new NullProgressMonitor();
            WorkspaceModifyOperation wmo = new WorkspaceModifyOperation(ResourcesPlugin.getWorkspace().getRoot()) {
              @Override
              protected void execute(final IProgressMonitor monitor) throws CoreException{
                  IProject project = CustomProjectSupport
                      .createBaseProject( name, location );
                  CustomProjectSupport.addNature( project, HaskellNature.NATURE_ID );
                  project.refreshLocal( IResource.DEPTH_INFINITE, null );

              }
            };
            try {
              // ResourcesPlugin.getWorkspace().run( operation, mon );
                 wmo.run( mon );
             } catch( Exception cex ) {
               HaskellCorePlugin.log(  UITexts.newYesodProjectWizard_error, cex );
             } finally {
               mon.done();
             }

          } catch( Exception e ) {
            HaskellCorePlugin.log(  UITexts.newYesodProjectWizard_error, e );
            Display.getDefault().asyncExec( new Runnable(){
              public void run() {
                MessageDialog
                .openError(
                    getShell(),
                    UITexts.newYesodProjectWizard_error_title,
                    UITexts.newYesodProjectWizard_error_message);
              }
            } );


          }
          return Status.OK_STATUS;
        }
      }.schedule();



    return true;
  }

}
