// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.wizards;

import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.common.ui.CommonUIPlugin;

import org.eclipse.core.runtime.*;
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
public class ProjectCreationWizard extends Wizard implements INewWizard,
		IExecutableExtension {

	private ProjectCreationWizardPage page;

	private IConfigurationElement configElement;
	
	private ProjectCreationInfo info;
	
	private ProjectCreationOperation fOperation;

	public ProjectCreationWizard(final ProjectCreationInfo info) {
		super();
		this.info = info;
		fOperation = createOperation();
		setDialogSettings(CommonUIPlugin.getDefault().getDialogSettings());
		setWindowTitle("Choose project name");
		setNeedsProgressMonitor(true);
	}

	protected ProjectCreationOperation createOperation() {
		return new ProjectCreationOperation();
	}

	public void addPages() {
		super.addPages();
		page = new ProjectCreationWizardPage(info);
		addPage(page);
	}

	public boolean performFinish() {
		fOperation.setProjectName(page.getProjectName());
		fOperation.setProjectLocation(page.getLocationPath().toString());

		IRunnableWithProgress op = new WorkspaceModifyDelegatingOperation(
				fOperation);
		boolean result = true;
		try {
			getContainer().run(false, true, op);
		} catch (InvocationTargetException e) {
			handleException(e.getTargetException());
			result = false;
		} catch (InterruptedException e) {
			result = false;
		}
		BasicNewProjectResourceWizard.updatePerspective(configElement);
		return result;
	}

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
		ImageDescriptor bannerImage = info.getBannerImage();
		if (bannerImage != null) {
			setDefaultPageImageDescriptor(bannerImage);
		}
	}

	private void handleException(final Throwable target) {
		String title = "A problem occured.";
		String message = "Could not create project.";
		if (target instanceof CoreException) {
			IStatus status = ((CoreException) target).getStatus();
			ErrorDialog.openError(getShell(), title, message, status);
			CommonUIPlugin.log(status);
		} else {
			MessageDialog.openError(getShell(), title, target.getMessage());
			CommonUIPlugin.log(target.getMessage(), target);
		}
	}
}