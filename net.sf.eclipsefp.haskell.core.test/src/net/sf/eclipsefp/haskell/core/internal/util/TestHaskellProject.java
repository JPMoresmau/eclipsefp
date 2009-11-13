package net.sf.eclipsefp.haskell.core.internal.util;

import java.io.ByteArrayInputStream;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;

public class TestHaskellProject implements ICorePreferenceNames {

	private IProject fUnderlyingProject;
	private IFolder fSourceFolder;

	public TestHaskellProject(final String projectName, final IEclipsePreferences corePrefs) throws CoreException {
	  setPreferences(corePrefs);
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

	private void setPreferences(final IEclipsePreferences corePrefs) {
	  corePrefs.put( SELECTED_COMPILER, "null" );
    corePrefs.put( FOLDERS_SRC, "src" );
  //  corePrefs.put( FOLDERS_OUT, "out" );
  //  corePrefs.put( TARGET_BINARY, "bin/theResult" );
    corePrefs.putBoolean( FOLDERS_IN_NEW_PROJECT, true );
  }

	public IFile createSourceFile(final String fileName, final String contents) throws CoreException {
		IFile file = fSourceFolder.getFile(fileName);
		file.create(new ByteArrayInputStream(contents.getBytes()), true, null);
		return file;
	}

	public void destroy() throws CoreException {
		fUnderlyingProject.delete(true, null);
	}

	public IProject getPlatformProject() {
		return fUnderlyingProject;
	}

}
