package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;

public class ProjectCreationOperation_PDETest
	extends ProjectCreationOperationPDETestCase {

	private static final String TMP_DIR = System.getProperty("java.io.tmpdir");

	public void testCreateProject()
		throws InvocationTargetException, InterruptedException, CoreException
	{
		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		assertFalse("Project already exists in the workspace", prj.exists());

		runOperation();

		prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		assertValid(prj);
	}

	public void testPlatformDefaultLocation()
		throws InvocationTargetException, InterruptedException, IOException
	{
		getOperation().setProjectLocation(Platform.getLocation().toString());

		runOperation();

		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		assertValid(prj);

		assertSameLocation(defaultLocation(), prj.getLocation().toString());
	}

	public void testCustomLocation()
		throws InvocationTargetException, InterruptedException, IOException
	{
		final String customLocation = TMP_DIR + '/' + PROJECT_NAME;
		getOperation().setProjectLocation(customLocation);

		runOperation();

		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		assertValid(prj);

		assertSameLocation(customLocation, prj.getLocation().toString());
	}

	public void testResortToDefaultLocationWhenNotInformed()
	    throws InvocationTargetException, InterruptedException, IOException
	{
		runOperation();

		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		assertSameLocation(defaultLocation(), prj.getLocation().toString());
	}

}
