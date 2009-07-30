package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.IOException;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithPreferences;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

public class ProjectCreationOperationPDETestCase extends TestCaseWithPreferences {

	protected static final String PROJECT_NAME = "hello.haskell.world";

	private IWorkspaceRoot fWorkspaceRoot;
	private ProjectCreationOperation fOperation;

	public ProjectCreationOperationPDETestCase() {
	  super();
	}

	@Override
	protected void setUp() throws Exception {
	  super.setUp();
		fWorkspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		fOperation = createOperation();
		fOperation.setProjectName(PROJECT_NAME);
	}

	protected ProjectCreationOperation createOperation() {
		ProjectCreationOperation result = new ProjectCreationOperation();
		result.setExtraOperation( new ProjectModelFilesOp() );
    return result;
	}

	protected ProjectCreationOperation getOperation() {
		return fOperation;
	}

	protected IWorkspaceRoot getWorkspaceRoot() {
		return fWorkspaceRoot;
	}

	protected void runOperation() {
		fOperation.run(new NullProgressMonitor());
	}

	protected static void assertValid(final IProject prj) {
		assertTrue("Project was not created", prj.exists());
		assertTrue("Project is closed", prj.isOpen());
	}

	protected String defaultLocation() {
		return Platform.getLocation().toString() + '/' + PROJECT_NAME;
	}

	protected void assertSameLocation(final String expected, final String actual) throws IOException {
		String expectedPath = new Path(expected).toFile().getCanonicalPath();
		String actualPath = new Path(actual).toFile().getCanonicalPath();
		assertEquals(expectedPath, actualPath);
	}

	@Override
	protected void tearDown() throws Exception {
		deleteCreatedProject();
		super.tearDown();
	}

	private void deleteCreatedProject() throws CoreException {
		IProject prj = fWorkspaceRoot.getProject(PROJECT_NAME);
		prj.delete(true, true, null);
	}

}
