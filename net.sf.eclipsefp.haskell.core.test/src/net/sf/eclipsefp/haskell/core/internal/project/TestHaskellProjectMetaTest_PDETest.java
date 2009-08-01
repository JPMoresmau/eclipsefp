package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import net.sf.eclipsefp.haskell.core.compiler.DefaultHaskellCompiler;
import net.sf.eclipsefp.haskell.core.internal.util.TestHaskellProject;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithPreferences;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

public class TestHaskellProjectMetaTest_PDETest extends TestCaseWithPreferences {

	public void testConstructorCreatesProject() throws CoreException {
		new TestHaskellProject("testing-project", getCorePrefs());

		IProject project = getProject("testing-project");
		assertNotNull(project);
		assertTrue(project.exists());
		assertHasHaskellNature(project);
		assertHasSourceFolder(project);
	}

	public void testCreatingSourceFiles() throws CoreException, IOException {
		TestHaskellProject project = new TestHaskellProject("another-project", getCorePrefs());

		project.createSourceFile("MyFile.txt", "These are the file contents\n");

		IProject underlyingProject = getProject("another-project");
		IFile file = underlyingProject.getFolder("src").getFile("MyFile.txt");
		assertNotNull(file);
		assertTrue(file.exists());
		assertEquals("These are the file contents", readFirstLine(file));
	}

	public void testDestroyProject() throws CoreException {
		TestHaskellProject project = new TestHaskellProject("other-project", getCorePrefs());

		IProject underlyingProject = getProject("other-project");
		assertNotNull(underlyingProject);
		assertTrue(underlyingProject.exists());

		project.destroy();

		underlyingProject = getProject("other-project");
		assertFalse(underlyingProject.exists());
	}

	public void testUsesDefaultCompiler() throws CoreException {
		TestHaskellProject project = new TestHaskellProject("yet-another-project", getCorePrefs());

		IHaskellProject hsprj = HaskellProjectManager.get(getProject("yet-another-project"));
		// TODO TtC this fails, but will soon be unnecessary with the new Build Configuration architecture
		assertEquals(DefaultHaskellCompiler.class, hsprj.getCompiler().getClass());

		project.destroy();
	}

	private String readFirstLine(final IFile file) throws IOException, CoreException {
		return new BufferedReader(new InputStreamReader(file.getContents())).readLine();
	}

	private void assertHasSourceFolder(final IProject project) {
		IFolder folder = project.getFolder("src");
		assertNotNull(folder);
		assertTrue(folder.exists());
	}

	private void assertHasHaskellNature(final IProject project) throws CoreException {
		if (null == project.getNature(HaskellNature.NATURE_ID)) {
			fail("Project doesn't have Haskell nature");
		}
	}

	private IProject getProject(final String projectName) {
		return ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
	}

}
