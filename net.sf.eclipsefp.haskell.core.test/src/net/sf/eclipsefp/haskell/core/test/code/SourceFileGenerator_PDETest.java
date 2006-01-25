package net.sf.eclipsefp.haskell.core.test.code;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;

import net.sf.eclipsefp.haskell.core.test.internal.doubles.MockCodeGenerator;

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
import de.leiffrenzel.fp.haskell.core.code.SourceFileGenerator;
import junit.framework.TestCase;

public class SourceFileGenerator_PDETest extends TestCase {
	
	private static final String PROJECT_NAME = "hello.haskell.world";
	private static final String SOURCE_FOLDER_NAME = "src";
	private static final String MODULE_NAME = "HelloHaskell";
	private static final String LITERATE_SUFFIX = ".lhs";
	private static final String USUAL_SUFFIX = ".hs";

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

	public void testCreateUsualStyleModule() throws CoreException, IOException {
		final String expectedContents = "module " + MODULE_NAME +  " where";

		//TODO try to use easymock here
		MockCodeGenerator codeGen = createCodeGenerator(expectedContents, fInfo.getCommentStyle());
		
		SourceFileGenerator fileGen = new SourceFileGenerator(codeGen);
		fileGen.createFile(new NullProgressMonitor(), fInfo);
		
		codeGen.verify();
		
		assertContents(expectedContents, MODULE_NAME + USUAL_SUFFIX);
	}
	
	public void testLiterateModule() throws InvocationTargetException, InterruptedException, CoreException, IOException {
		final String expectedContents = "> module " + MODULE_NAME +  " where";

		fInfo.setCommentStyle(EHaskellCommentStyle.LITERATE);
		MockCodeGenerator codeGen = createCodeGenerator(expectedContents, fInfo.getCommentStyle());
		
		SourceFileGenerator fileGen = new SourceFileGenerator(codeGen);
		fileGen.createFile(new NullProgressMonitor(), fInfo);
		
		codeGen.verify();

		assertContents(expectedContents, MODULE_NAME + LITERATE_SUFFIX);
	}
	
	public void testTexStyleModule() throws InvocationTargetException, InterruptedException, CoreException, IOException {
		final String expectedContents = "\\begin{code}\n" +
				                        "module " + MODULE_NAME +  " where\n" +
				                        "\\end{code}";

		fInfo.setCommentStyle(EHaskellCommentStyle.TEX);
		MockCodeGenerator codeGen = createCodeGenerator(expectedContents, fInfo.getCommentStyle());
		
		SourceFileGenerator fileGen = new SourceFileGenerator(codeGen);
		fileGen.createFile(new NullProgressMonitor(), fInfo);
		
		codeGen.verify();

		assertContents(expectedContents, MODULE_NAME + LITERATE_SUFFIX);
	}

	@Override
	protected void tearDown() throws Exception {
		fProject.delete(true, null);
	}

	private MockCodeGenerator createCodeGenerator(
		final String expectedContents, EHaskellCommentStyle expectedStyle)
	{
		MockCodeGenerator codeGen = new MockCodeGenerator();
		codeGen.setOutput(expectedContents);
		codeGen.setExpectedStyle(expectedStyle);

		return codeGen;
	}

	private IFolder createFolder(IProject project, String name) throws CoreException {
		IFolder srcFldr = project.getFolder(name);
		if (srcFldr.exists()) {
			fail("Folder " + project.getName() + "/" + name +
				 " already exists. Please check the test code.");
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

	private void assertContents(String expected, String fileName) throws CoreException, IOException {
		IFile file = fSrcFolder.getFile(fileName);
		assertTrue(file.exists());
		
		BufferedReader input = new BufferedReader(
								new InputStreamReader(file.getContents()));
		char[] inBuffer = new char[expected.length()];
		int n = input.read(inBuffer);
		input.close();
		
		final String actual = new String(inBuffer, 0, n);
		assertEquals(expected, actual);
	}
}
