// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperationPDETestCase;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Preferences;

public class HaskellProjectCreationOperation_PDETest extends
		ProjectCreationOperationPDETestCase {

	private Preferences fStore;

	@Override
  protected ProjectCreationOperation createOperation() {
		fStore = new Preferences();
		fStore.setValue(ICorePreferenceNames.FOLDERS_IN_NEW_PROJECT, true);
		return new HaskellProjectCreationOperation(fStore);
	}

	public void testAddsHaskellNature() throws InvocationTargetException,
			InterruptedException, CoreException {
		runOperation();

		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		assertNotNull(prj.getNature(HaskellNature.NATURE_ID));
	}

	public void testCreatesDirectoriesFromPreferences()
			throws InvocationTargetException, InterruptedException {
		fStore.setValue(ICorePreferenceNames.FOLDERS_SRC, "customSrc");
		fStore.setValue(ICorePreferenceNames.FOLDERS_OUT, "customOut");
		fStore.setValue(ICorePreferenceNames.FOLDERS_BIN, "customBin");

		runOperation();
		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);

		assertValid(prj.getFolder("customSrc"));
		assertValid(prj.getFolder("customOut"));
		assertValid(prj.getFolder("customBin"));
	}

	public void testCreatesDescriptorFile() throws InvocationTargetException,
			InterruptedException {
		runOperation();
		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		IFile f = prj.getFile(HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR);
		assertValid(f);
	}

	public void testSetsUpProjectFoldersFromPreferences()
			throws InvocationTargetException, InterruptedException,
			CoreException, IOException {
		fStore.setValue(ICorePreferenceNames.FOLDERS_SRC, "mySrc");
		fStore.setValue(ICorePreferenceNames.FOLDERS_OUT, "myOut");
		fStore.setValue(ICorePreferenceNames.FOLDERS_BIN, "myBin");
		fStore.setValue(ICorePreferenceNames.TARGET_BINARY, "myBinary");
		fStore.setValue(ICorePreferenceNames.SELECTED_COMPILER, "null");

		runOperation();
		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		IFile f = prj.getFile(HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR);
		final String expectedContents = HaskellProjectManager
				.createDescriptorContent("mySrc", "myOut", "myBin", "myBinary", "null");
		assertEquals(expectedContents, readContents(f));

	}

	public void testDoNotCreateFoldersWhenPreferenceDisabled()
			throws InvocationTargetException, InterruptedException,
			CoreException, IOException {
		fStore.setValue(ICorePreferenceNames.FOLDERS_IN_NEW_PROJECT, false);

		runOperation();

		IProject prj = getWorkspaceRoot().getProject(PROJECT_NAME);
		//should only contain the project descriptors (.project and .hsproject)
		assertEquals(2, prj.members().length);

		IFile f = prj.getFile(HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR);
		assertValid(f);
		assertEquals("", readContents(f));
	}

	private String readContents(final IFile file) throws CoreException, IOException {
		StringBuffer buf = new StringBuffer(1024);
		InputStream input = file.getContents();
		BufferedReader reader = new BufferedReader(new InputStreamReader(input));
		String line;
		while (null != (line = reader.readLine())) {
			buf.append(line);
			buf.append('\n');
		}
		input.close();
		return buf.toString();
	}

	private void assertValid(final IResource res) {
		assertNotNull(res);
		assertTrue("Resource does not exist", res.exists());
	}

}
