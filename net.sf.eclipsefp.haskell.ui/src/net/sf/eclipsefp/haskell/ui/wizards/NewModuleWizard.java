// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;


/** <p>The wizard for creating a new Haskell module.</p>
  * 
  * @author Leif Frenzel
  */
public class NewModuleWizard extends Wizard implements INewWizard {

  public static final String ID = NewModuleWizard.class.getName();
  
  private IStructuredSelection selection;
  private NewModuleWizardPage page;
  
  public NewModuleWizard() {
    super();
    setNeedsProgressMonitor( true );
    setWindowTitle( "New Haskell Module" );
    initBannerImage();
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
  }

  public void init( final IWorkbench workbench, 
                    final IStructuredSelection selection ) {
    this.selection = selection ;
  }
  
  @Override
  public void addPages() {
    super.addPages();
    page = new NewModuleWizardPage();
    addPage( page );
    page.init( selection );
  }

  @Override
  public boolean performFinish() {
    ModuleCreationOperation mco = page.getOperation();
    IRunnableWithProgress op = new WorkspaceModifyDelegatingOperation( mco );
    boolean result = false;
    try {
      getContainer().run( false, true, op );
      result = true;
      finish( mco.getGeneratedFile() );
    } catch( Exception ex ) {
      handleFinishException( ex );
      HaskellUIPlugin.log( "Error creating new module.", ex );
    }
    return result;
  }

  
  // helping methods
  //////////////////
  
  private void handleFinishException( final Exception ex ) {
    String msg =   "The following error occured: " 
                 + ex.getLocalizedMessage() 
                 + "Please see workspace/.metadata/.log for more information.";
    MessageDialog.openError( getShell(), "Problem occured", msg );
  }

  private void finish( final IFile createdFile ) {
    if( createdFile != null ) {
      selectAndReveal( createdFile );
      openResource( createdFile );
    }
  }

  private void selectAndReveal( final IResource newResource ) {
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
  
  private void openResource( final IFile resource ) {
    final IWorkbenchPage activePage = getPage();
    if( activePage != null ) {
      final Display display = getShell().getDisplay();
      if( display != null ) {
        display.asyncExec( new Runnable() {
          public void run() {
            try {
              IDE.openEditor( activePage, resource, true );
            } catch( PartInitException pie ) {
              String msg =  "Could not open editor for file '"
                           + resource.getName() + "'.";
              HaskellUIPlugin.log( msg, pie );
            }
          }
        } );
      }
    }
  }
  
  private void initBannerImage() {
    String key = IImageNames.NEW_MODULE;
    ImageDescriptor imageDescriptor = HaskellUIImages.getImageDescriptor( key );
    setDefaultPageImageDescriptor( imageDescriptor );
  }
}