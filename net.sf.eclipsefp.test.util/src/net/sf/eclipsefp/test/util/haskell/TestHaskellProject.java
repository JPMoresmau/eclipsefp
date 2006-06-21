package net.sf.eclipsefp.test.util.haskell;

import java.io.StringBufferInputStream;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.ui.wizards.HaskellProjectCreationOperation;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;

public class TestHaskellProject {

	private IProject fUnderlyingProject;
	private IFolder fSourceFolder;
	
	public TestHaskellProject(final String projectName) throws CoreException {
		ProjectCreationOperation op = new HaskellProjectCreationOperation();
		op.setProjectName(projectName);
        try {
			op.run(new NullProgressMonitor());
			fUnderlyingProject = ResourcesPlugin.getWorkspace().getRoot().
			                         getProject(projectName);
			fSourceFolder = fUnderlyingProject.getFolder("src");
		} catch (Exception e) {
			throw new CoreException(Status.OK_STATUS);
		}
	}

	@SuppressWarnings("deprecation")
	public IFile createSourceFile(String fileName, final String contents) throws CoreException {
		IFile file = fSourceFolder.getFile(fileName);
		file.create(new StringBufferInputStream(contents), true, null);
		return file;
	}
	
	public void destroy() throws CoreException {
		fUnderlyingProject.delete(true, null);
	}

	public IProject getPlatformProject() {
		return fUnderlyingProject;
	}

}
