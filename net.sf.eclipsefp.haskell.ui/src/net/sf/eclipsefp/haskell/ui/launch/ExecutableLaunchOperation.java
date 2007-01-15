// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.launch;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.sf.eclipsefp.haskell.core.launch.ILaunchAttributes;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;

/**
 * <p>
 * encapsulates the work involved in finding a launch configuration (if one
 * exists) for some element and launching it.
 * </p>
 * 
 * @author Leif Frenzel
 */
class ExecutableLaunchOperation extends LaunchOperation {

	void launch(final IResource resource, final IProgressMonitor monitor)
			throws CoreException {
		if (resource != null) {
			IProject project = resource.getProject();
			if (project.hasNature(HaskellNature.NATURE_ID)) {
				IFile executable = findExecutable(resource);
				ILaunchConfiguration configuration = getConfiguration(executable);
				if (configuration != null) {
					configuration.launch(ILaunchManager.RUN_MODE, monitor);
				}
			}
		}
	}

	// helping methods
	// ////////////////

	private IFile findExecutable(final IResource res) throws CoreException {
		IFile result = ResourceUtil.getProjectExecutable(res.getProject());
		if (result == null) {
			String msg = "Could not determine executable for selection: "
					+ res.getName();
			Status status = new Status(IStatus.ERROR,
					                   HaskellUIPlugin.getPluginId(),
					                   IStatus.ERROR, msg, null);
			throw new CoreException(status);
		}
		return result;
	}

	private ILaunchConfiguration getConfiguration(final IFile file)
			throws CoreException {
		List<ILaunchConfiguration> configurations = findConfiguration(file);
		ILaunchConfiguration result = null;
		int count = configurations.size();
		if (count < 1) {
			// If there are no existing configs associated with the
			// ICompilationUnit,
			// create one.
			result = createConfiguration(file);
		} else if (count == 1) {
			// If there is exactly one config associated with the
			// ICompilationUnit,
			// return it.
			result = configurations.get(0);
		} else {
			// Otherwise, if there is more than one config associated with the
			// ICompilationUnit, prompt the user to choose one.
			result = chooseConfiguration(configurations);
		}
		return result;
	}

	private ILaunchConfiguration createConfiguration(final IFile executable)
			throws CoreException {

		ILaunchConfigurationType configType = getConfigType();
		String id = createConfigId(executable);
		ILaunchConfigurationWorkingCopy wc = configType.newInstance(null, id);
		String exePath = getExePath(executable);
		wc.setAttribute(ILaunchAttributes.EXECUTABLE, exePath);
		String projectName = ILaunchAttributes.PROJECT_NAME;
		wc.setAttribute(projectName, executable.getProject().getName());
		return wc.doSave();
	}

	private String getExePath(final IFile executable) {
		String result = executable.getLocation().toOSString();
		if (!new File(result).exists()) {
			String msg = "Could not locate the project executable - supposed to be "
					+ result;
			HaskellUIPlugin.log(msg, IStatus.ERROR);
		}
		return result;
	}

	private List<ILaunchConfiguration> findConfiguration(final IFile file) throws CoreException {
		List<ILaunchConfiguration> result = Collections.emptyList();
		ILaunchConfiguration[] configurations = getConfigurations();
		result = new ArrayList<ILaunchConfiguration>(configurations.length);
		for (int i = 0; i < configurations.length; i++) {
			ILaunchConfiguration configuration = configurations[i];
			String exePath = getExePath(file);
			String projectName = file.getProject().getName();
			if (getExePath(configuration).equals(exePath)
					&& getProjectName(configuration).equals(projectName)) {
				result.add(configuration);
			}
		}
		return result;
	}

	private String createConfigId(final IFile file) {
		String name = file.getName();
		return getLaunchManager().generateUniqueLaunchConfigurationNameFrom(
				name);
	}
}