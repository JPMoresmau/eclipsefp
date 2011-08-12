package net.sf.eclipsefp.haskell.ui.wizards;


import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;

public abstract class RevealAtEndWizard extends Wizard {

  public RevealAtEndWizard() {
    super();
  }

  protected void selectAndReveal( final IResource newResource ) {
    IWorkbenchPage page = getPage();
    if( page != null ) {
      IWorkbenchWindow workbenchWindow = page.getWorkbenchWindow();
      BasicNewResourceWizard.selectAndReveal( newResource, workbenchWindow );
    }
  }

  private IWorkbenchPage getPage() {
    IWorkbenchPage result = null;
    IWorkbench workbench = PlatformUI.getWorkbench();
    IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
    if( window != null ) {
      result = window.getActivePage();
    }
    return result;
  }

  protected void openResource( final IFile resource ) {
    final IWorkbenchPage activePage = getPage();
    if( activePage != null ) {
      final Display display = getShell().getDisplay();
      if( display != null ) {
        display.asyncExec( new Runnable() {

          public void run() {
            try {
              IDE.openEditor( activePage, resource, true );
            } catch( PartInitException pie ) {
              String msg = "Could not open editor for file '"
                  + resource.getName() + "'.";
              HaskellUIPlugin.log( msg, pie );
            }
          }
        } );
      }
    }
  }

}