// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.wizards;

import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.common.core.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;

/**
 * <p>
 * a wizard for creating a new project. The creation is similar to a new project
 * creation in the resource ui, but adds a nature for a specific fp feature.
 * </p>
 * 
 * @author Leif Frenzel
 */
public abstract class ProjectCreationWizard
    extends Wizard
    implements INewWizard, IExecutableExtension {

	private ProjectCreationWizardPage page;

	private IConfigurationElement configElement;
	
	private final ProjectCreationOperation fOperation;

	public ProjectCreationWizard(final ProjectCreationOperation operation) {
		super();
		fOperation = operation;
		setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
		setWindowTitle("Choose project name");
		setNeedsProgressMonitor(true);
	}

	@Override
  public void addPages() {
  	    super.addPages();
	    page = new ProjectCreationWizardPage(
                       getPageTitle(), getPageDescrition());
	    addPage(page);
	}

	@Override
  public boolean performFinish() {
    IRunnableWithProgress rwp = configureOperation();
    IRunnableWithProgress op = new WorkspaceModifyDelegatingOperation( rwp );
    boolean result = true;
    try {
      getContainer().run( false, true, op );
    } catch( InvocationTargetException e ) {
      handleException( e.getTargetException() );
      result = false;
    } catch( InterruptedException e ) {
      result = false;
    }
    BasicNewProjectResourceWizard.updatePerspective( configElement );
    return result;
  }
    
    /**
     * Returns a descriptor for the banner image (the one on the top right
     * corner)
     */
    protected abstract ImageDescriptor getBannerImage();

    /**
     * Returns the description for the create project wizard page
     */
    protected abstract String getPageDescrition();

    /**
     * Returns the title for the create project wizard page
     */
    protected abstract String getPageTitle();

    /**
	 * Stores the configuration element for the wizard. The config element will
	 * be used in <code>performFinish</code> to set the result perspective.
	 */
	public void setInitializationData(
			final IConfigurationElement configElement,
			final String propertyName, final Object data) {
		this.configElement = configElement;
	}

	public void init(final IWorkbench workbench,
			final IStructuredSelection selection) {
		initializePageImageDescriptor();
	}

	// helping methods
  // ////////////////

  private void initializePageImageDescriptor() {
    ImageDescriptor bannerImage = getBannerImage();
    if( bannerImage != null ) {
      setDefaultPageImageDescriptor( bannerImage );
    }
  }

  private void handleException( final Throwable target ) {
    String title = "A problem occured.";
    String message = "Could not create project.";
    if( target instanceof CoreException ) {
      IStatus status = ( ( CoreException )target ).getStatus();
      ErrorDialog.openError( getShell(), title, message, status );
      HaskellUIPlugin.log( status.getMessage(), status.getSeverity() );
    } else {
      MessageDialog.openError( getShell(), title, target.getMessage() );
      HaskellUIPlugin.log( target.getMessage(), target );
    }
  }
  
  private IRunnableWithProgress configureOperation() {
    fOperation.setProjectName( page.getProjectName() );
    fOperation.setProjectLocation( page.getLocationPath().toString() );

    IRunnableWithProgress rwp = new IRunnableWithProgress() {
      public void run( final IProgressMonitor monitor ) {
        fOperation.run( monitor );
      }
    };
    return rwp;
  }
}