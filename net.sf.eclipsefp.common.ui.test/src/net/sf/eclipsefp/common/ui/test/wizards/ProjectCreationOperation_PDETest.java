package net.sf.eclipsefp.common.ui.test.wizards;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationInfo;
import net.sf.eclipsefp.common.ui.wizards.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

import junit.framework.TestCase;

public class ProjectCreationOperation_PDETest extends TestCase {
	
	private static final String TMP_DIR = System.getProperty("java.io.tmpdir");
	private static final String PROJECT_NAME = "hello.haskell.world";

	private ProjectCreationInfo fInfo;
	private IWorkspaceRoot fWorkspaceRoot;
	
	@Override
	protected void setUp() {
		fWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		fInfo = new DumbProjectCreationInfo();
		fInfo.setProjectName(PROJECT_NAME);
	}

	public void testCreateProject() throws InvocationTargetException, InterruptedException, CoreException {
		fInfo.setProjectName(PROJECT_NAME);
		
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		
		assertFalse("Project already exists in the workspace", prj.exists());
		
		runOperation(new ProjectCreationOperation(fInfo));
		
		prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		assertValid(prj);
	}
	
	public void testPlatformDefaultLocation() throws InvocationTargetException, InterruptedException, IOException {
		final String defaultLocation = Platform.getLocation().toString() + '/' + PROJECT_NAME;
		fInfo.setProjectLocation(Platform.getLocation().toString());
		
		runOperation(new ProjectCreationOperation(fInfo));
		
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		assertValid(prj);
		
		assertSameLocation(defaultLocation, prj.getLocation().toString());
	}

	public void testCustomLocation() throws InvocationTargetException, InterruptedException, IOException {
		final String customLocation = TMP_DIR + '/' + PROJECT_NAME;
		fInfo.setProjectLocation(customLocation);
		
		runOperation(new ProjectCreationOperation(fInfo));
		
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		assertValid(prj);
		
		assertSameLocation(customLocation, prj.getLocation().toString());
	}
	
	public void testAddsHaskellNature()
		throws InvocationTargetException, InterruptedException, CoreException
	{
		runOperation(new ProjectCreationOperation(fInfo));
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		assertNotNull(prj.getNature(HaskellNature.NATURE_ID));
	}
	
	public void testSetsUpSourceLocation()
		throws InvocationTargetException, InterruptedException
	{
		runOperation(new ProjectCreationOperation(fInfo));
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		IHaskellProject hprj = HaskellProjectManager.get(prj);
		System.out.println(hprj.getSourcePath().toString());
	}

	@Override
	protected void tearDown() throws Exception {
		deleteCreatedProject();
	}


	private void runOperation(ProjectCreationOperation op) throws InvocationTargetException, InterruptedException {
		op.run(new NullProgressMonitor());
	}
	
	private static void assertValid(IProject prj) {
		assertTrue("Project was not created", prj.exists());
		assertTrue("Project is closed", prj.isOpen());
	}
	
	private void deleteCreatedProject() throws CoreException {
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		prj.delete(true, true, null);
	}

	private void assertSameLocation(final String expected, final String actual) throws IOException {
		String expectedPath = new Path(expected).toFile().getCanonicalPath();
		String actualPath = new Path(actual).toFile().getCanonicalPath();
		assertEquals(expectedPath, actualPath);
	}
	
}
