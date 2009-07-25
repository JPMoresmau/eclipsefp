// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.wizards;

import java.lang.reflect.InvocationTargetException;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
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
import org.eclipse.ui.wizards.newresource.BasicNewResourceWizard;

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

	private IWorkbench workbench;

	protected NewProjectWizardPage page;

	private IConfigurationElement configElement;

	private final ProjectCreationOperation operation;

	public ProjectCreationWizard( final ProjectCreationOperation operation ) {
    super();
    this.operation = operation;
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
    setWindowTitle( getTheWindowTitle() );
    setNeedsProgressMonitor( true );
  }

	@Override
  public void addPages() {
    page = new NewProjectWizardPage();
    page.setTitle( getPageTitle() );
    page.setDescription( getPageDescription() );
    addPage( page );
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
    BasicNewResourceWizard.selectAndReveal(page.getProjectHandle(), workbench.getActiveWorkbenchWindow());
    return result;
  }

  protected abstract ImageDescriptor getBannerImage();

  protected abstract String getPageDescription();

  protected abstract String getPageTitle();

  protected abstract String getTheWindowTitle();

  protected ProjectCreationOperation getOperation() {
    return operation;
  }

  /**
	  * Stores the configuration element for the wizard. The config element will
	  * be used in <code>performFinish</code> to set the result perspective.
	  */
	public void setInitializationData(
	    final IConfigurationElement configElement,
      final String propertyName,
      final Object data ) {
    this.configElement = configElement;
  }

  public void init(
      final IWorkbench workbench,
      final IStructuredSelection selection ) {
	this.workbench = workbench;
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
    operation.setProjectName( page.getProjectName() );
    operation.setProjectLocation( page.getProjectLocationPath().toString() );

    IRunnableWithProgress rwp = new IRunnableWithProgress() {
      public void run( final IProgressMonitor monitor ) {
        operation.run( monitor );
      }
    };
    return rwp;
  }
}