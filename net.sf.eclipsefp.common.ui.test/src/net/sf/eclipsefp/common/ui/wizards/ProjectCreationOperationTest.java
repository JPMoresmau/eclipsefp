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
	
	private static final String PROJECT_NAME = "hello.haskell.world";

	private ProjectCreationInfo fInfo;
	
	@Override
	protected void setUp() {
		fInfo = new DumbProjectCreationInfo();
	}

	public void testCreateProject() throws InvocationTargetException, InterruptedException, CoreException {
		
		fInfo.setProjectName(PROJECT_NAME);
		
		IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		IProject prj = root.getProject(PROJECT_NAME);
		
		assertFalse("Project already exists in the workspace", prj.exists());
		
		runOperation(new ProjectCreationOperation(fInfo));
		
		prj = root.getProject(PROJECT_NAME);
		assertTrue("Project was not created", prj.exists());
		assertTrue("Project is closed", prj.isOpen());
		
		prj.delete(true, true, null);
	}

	private void runOperation(ProjectCreationOperation op) throws InvocationTargetException, InterruptedException {
		op.run(new NullProgressMonitor());
	}
	
}
