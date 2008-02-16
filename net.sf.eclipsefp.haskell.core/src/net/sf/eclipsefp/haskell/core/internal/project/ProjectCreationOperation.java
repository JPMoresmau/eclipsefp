// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;

/**
 * <p>
 * creates the new project.
 * </p>
 *
 * @author Leif Frenzel
 */
public class ProjectCreationOperation {

	private String fProjectName;
	private String fLocation;

	public void run( final IProgressMonitor passedMon ) {
		IProgressMonitor mon = (passedMon == null) ? new NullProgressMonitor()
				: passedMon;
		IWorkspaceRunnable operation = new IWorkspaceRunnable() {
			public void run(final IProgressMonitor monitor)
					throws CoreException {
			  String msg = CoreTexts.projectCreationOperation_creating;
				monitor.beginTask( msg, getDirectories().length + 3);

				monitor.subTask( CoreTexts.projectCreationOperation_init );
				IProject project = createProjectResource();
				monitor.worked(1);

				monitor.subTask( CoreTexts.projectCreationOperation_natures );
				addNatures(monitor, project);

				monitor.subTask( CoreTexts.projectCreationOperation_dirs );
				createDirectories(monitor, project);

				monitor.subTask( CoreTexts.projectCreationOperation_settings );
				createDescriptionFile(monitor, project);
			}
		};
		try {
			ResourcesPlugin.getWorkspace().run(operation, mon);
		} catch (CoreException cex) {
			HaskellCorePlugin.log("Problem creating new project.", cex); //$NON-NLS-1$
		} finally {
			mon.done();
		}
	}

	public void setProjectName(final String name) {
		fProjectName = name;
	}

	public String getProjectName() {
		return fProjectName;
	}

	public void setProjectLocation(final String location) {
		fLocation = location;
	}

	public String getProjectLocation() {
		return fLocation;
	}

	// helping methods
	// ////////////////

	private IProject createProjectResource() throws CoreException {
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final String projectName = getProjectName();
		final String projectLocation = getProjectLocation();

		IProject result = root.getProject(projectName);
		IProjectDescription desc = null;

		if (!isDefaultLocation(projectLocation)) {
			desc = result.getWorkspace().newProjectDescription(projectName);
			desc.setLocation(new Path(projectLocation));
		}

		if (!result.exists()) {
			result.create(desc, null);
		}
		if (!result.isOpen()) {
			result.open(null);
		}
		return result;
	}

	private boolean isDefaultLocation(final String projectLocation) {
		return null == projectLocation || "".equals(projectLocation) //$NON-NLS-1$
				|| Platform.getLocation().toString().equals(projectLocation);
	}

	private void addNatures(final IProgressMonitor mon, final IProject project)
			throws CoreException {
		IProjectDescription desc = project.getDescription();
		desc.setNatureIds(getProjectNatures());
		project.setDescription(desc, new SubProgressMonitor(mon, 1));
	}

	/**
	 * Returns an array of project nature ids to be added to the created
	 * project.
	 *
	 * This method should be overriden by clients.
	 */
	protected String[] getProjectNatures() {
		return new String[0];
	}

	/**
	 * Returns an array of directory names to be created inside the project.
	 *
	 * This method should be overriden by clients.
	 */
	protected String[] getDirectories() {
		return new String[0];
	}

	/**
	 * Returns an object describing the project descriptor file
	 *
	 * This method should be overriden by clients.
	 */
	protected DescriptorFileInfo getDescFileInfo() {
		return null;
	}

	private void createDirectories(final IProgressMonitor mon,
			final IProject proj) throws CoreException {
		String[] directories = getDirectories();
		for (int i = 0; i < directories.length; i++) {
			if (!"".equals(directories[i])) { //$NON-NLS-1$
				IFolder dir = proj.getFolder(directories[i]);
				dir.create(true, true, new SubProgressMonitor(mon, 1));
			}
		}
	}

	private void createDescriptionFile(final IProgressMonitor monitor,
			final IProject project) throws CoreException {
		DescriptorFileInfo descFileInfo = getDescFileInfo();
		if (descFileInfo != null && !"".equals(descFileInfo.getName())) { //$NON-NLS-1$
			IFile file = project.getFile(descFileInfo.getName());
			String content = descFileInfo.getContent();
			InputStream is = new ByteArrayInputStream(content.getBytes());
			file.create(is, true, new SubProgressMonitor(monitor, 1));
		}
	}

}