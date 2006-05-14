package net.sf.eclipsefp.test.util.haskell.metatest;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.test.util.haskell.TestHaskellProject;
import junit.framework.TestCase;

public class TestHaskellProjectMetaTest_PDETestCase extends TestCase {
	
	public void testConstructorCreatesProject() throws CoreException {
		new TestHaskellProject("testing-project");
		
		IProject project = getProject("testing-project");
		assertNotNull(project);
		assertTrue(project.exists());
		assertHasHaskellNature(project);
		assertHasSourceFolder(project);
	}
	
	public void testCreatingSourceFiles() throws CoreException, IOException {
		TestHaskellProject project = new TestHaskellProject("another-project");
		
		project.createSourceFile("MyFile.txt", "These are the file contents\n");
		
		IProject underlyingProject = getProject("another-project");
		IFile file = underlyingProject.getFolder("src").getFile("MyFile.txt");
		assertNotNull(file);
		assertTrue(file.exists());
		assertEquals("These are the file contents", readFirstLine(file));
	}
	
	public void testDestroyProject() throws CoreException {
		TestHaskellProject project = new TestHaskellProject("other-project");
		
		IProject underlyingProject = getProject("other-project");
		assertNotNull(underlyingProject);
		assertTrue(underlyingProject.exists());
		
		project.destroy();
		
		underlyingProject = getProject("other-project");
		assertFalse(underlyingProject.exists());
	}

	private String readFirstLine(IFile file) throws IOException, CoreException {
		return new BufferedReader(new InputStreamReader(file.getContents())).readLine();
	}

	private void assertHasSourceFolder(IProject project) {
		IFolder folder = project.getFolder("src");
		assertNotNull(folder);
		assertTrue(folder.exists());
	}

	private void assertHasHaskellNature(IProject project) throws CoreException {
		if (null == project.getNature(HaskellNature.NATURE_ID)) {
			fail("Project doesn't have Haskell nature");
		}
	}

	private IProject getProject(String projectName) {
		return ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
	}

}
