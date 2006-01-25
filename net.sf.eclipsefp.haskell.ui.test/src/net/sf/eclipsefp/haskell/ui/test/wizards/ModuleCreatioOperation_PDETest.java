package net.sf.eclipsefp.haskell.ui.test.wizards;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;

import de.leiffrenzel.fp.haskell.core.code.EHaskellCommentStyle;
import de.leiffrenzel.fp.haskell.core.code.ModuleCreationInfo;
import de.leiffrenzel.fp.haskell.ui.wizards.ModuleCreationOperation;
import junit.framework.TestCase;

public class ModuleCreatioOperation_PDETest extends TestCase {
	
	private static final String LITERATE_SUFFIX = ".lhs";
	private static final String USUAL_SUFFIX = ".hs";
	private static final String MODULE_NAME = "HelloHaskell";
	private static final String PROJECT_NAME = "hello.haskell.world";
	private static final String SOURCE_FOLDER_NAME = "src";

	private IProject fProject;
	private IFolder fSrcFolder;
	private ModuleCreationInfo fInfo;

	@Override
	protected void setUp() throws Exception {
		fProject = createProject(PROJECT_NAME);
		fSrcFolder = createFolder(fProject, SOURCE_FOLDER_NAME);
		fInfo = new ModuleCreationInfo();
		fInfo.setFolders(new Path(""));
		fInfo.setSourceContainer(fSrcFolder);
		fInfo.setModuleName(MODULE_NAME);
	}

	public void testCreateModule() throws CoreException, InvocationTargetException, InterruptedException, IOException {
		runOperation(new ModuleCreationOperation(fInfo));
		
		IFile file = fSrcFolder.getFile(MODULE_NAME + USUAL_SUFFIX);
		assertTrue(file.exists());
		
		assertContents("\nmodule " + MODULE_NAME + " where\n", file);
	}

	private void assertContents(String expected, IFile file) throws CoreException, IOException {
		BufferedReader input = new BufferedReader(
								new InputStreamReader(file.getContents()));
		char[] inBuffer = new char[expected.length()];
		int n = input.read(inBuffer);
		input.close();
		
		final String actual = new String(inBuffer, 0, n);
		assertEquals(expected, actual);
	}

	private void runOperation(final ModuleCreationOperation op) throws InvocationTargetException, InterruptedException {
		op.run(new NullProgressMonitor());
	}
	
	public void testLiterateModule() throws InvocationTargetException, InterruptedException, CoreException, IOException {
		fInfo.setCommentStyle(EHaskellCommentStyle.LITERATE);
		
		runOperation(new ModuleCreationOperation(fInfo));
		
		IFile file = fSrcFolder.getFile(MODULE_NAME + LITERATE_SUFFIX);
		assertTrue(file.exists());
		
		//TODO Mock this!!!
		assertContents("\n> module " + MODULE_NAME + " where\n", file);
	}
	
	public void testTexStyleModule() throws InvocationTargetException, InterruptedException, CoreException, IOException {
		fInfo.setCommentStyle(EHaskellCommentStyle.TEX);

		runOperation(new ModuleCreationOperation(fInfo));
		
		IFile file = fSrcFolder.getFile(MODULE_NAME + LITERATE_SUFFIX);
		assertTrue(file.exists());
		
		//TODO Mock this!!!
		final String expectedContents = "\n\\begin{code}\n" +
				                        "module " + MODULE_NAME + " where\n" +
				                        "\\end{code}";
		assertContents(expectedContents, file);
	}
	
	@Override
	protected void tearDown() throws Exception {
		fProject.delete(true, null);
	}

	private IFolder createFolder(IProject project, String name) throws CoreException {
		IFolder srcFldr = project.getFolder(name);
		if (srcFldr.exists()) {
			fail("Folder " + project.getName() + "/" + name +
				 " already existis. Please check the test code.");
		}
		srcFldr.create(true, true, null);

		return srcFldr;
	}

	private IProject createProject(String name) throws CoreException {
		IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		IProject project = workspaceRoot.getProject(name);
		if (project.exists()) {
			fail("Project " + name + "already exists. Please check the test code.");
		}
		project.create(null);
		project.open(null);
		
		return project;
	}
	
}
