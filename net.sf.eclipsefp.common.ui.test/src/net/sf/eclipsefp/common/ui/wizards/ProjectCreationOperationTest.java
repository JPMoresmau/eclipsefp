package net.sf.eclipsefp.common.ui.wizards;

import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationInfo;
import net.sf.eclipsefp.common.ui.wizards.ProjectCreationOperation;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;

import junit.framework.TestCase;

public class ProjectCreationOperationTest extends TestCase {
	
	private static final String TMP_DIR = System.getenv("TEMP");
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
		
		prj.delete(true, true, null);
	}

	public void testCustomLocation() throws InvocationTargetException, InterruptedException {
		final String customLocation = TMP_DIR + '/' + PROJECT_NAME;
		fInfo.setProjectLocation(customLocation);
		
		runOperation(new ProjectCreationOperation(fInfo));
		
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		assertValid(prj);
		
		assertEquals("Incorrect project location", customLocation, prj.getLocation().toString());
	}

	private void runOperation(ProjectCreationOperation op) throws InvocationTargetException, InterruptedException {
		op.run(new NullProgressMonitor());
	}
	
	private static void assertValid(IProject prj) {
		assertTrue("Project was not created", prj.exists());
		assertTrue("Project is closed", prj.isOpen());
	}
	
}
