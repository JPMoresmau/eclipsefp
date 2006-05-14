package net.sf.eclipsefp.test.util.haskell;

import java.io.StringBufferInputStream;

import net.sf.eclipsefp.haskell.core.project.HaskellNature;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

public class TestHaskellProject {

	private IProject fUnderlyingProject;
	private IFolder fSourceFolder;
	
	public TestHaskellProject(final String projectName) throws CoreException {
	    IWorkspaceRunnable op = new IWorkspaceRunnable() {
	        public void run( final IProgressMonitor monitor ) throws CoreException {
	          IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
	          fUnderlyingProject = root.getProject(projectName);
	          fUnderlyingProject.create( null );
	          fUnderlyingProject.open( null );
	          
	          setHaskellNature();
	          createSourceFolder();
	        }

	      };
	      ResourcesPlugin.getWorkspace().run( op, null, 0, null );
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

	private void setHaskellNature() throws CoreException {
		IProjectDescription description = fUnderlyingProject.getDescription();
		description.setNatureIds(new String[] { HaskellNature.NATURE_ID });
		fUnderlyingProject.setDescription(description, null);
	}

	private void createSourceFolder() throws CoreException {
		fSourceFolder = fUnderlyingProject.getFolder("src");
		fSourceFolder.create(true, true, null);
	}

	public IProject getPlatformProject() {
		return fUnderlyingProject;
	}

}
