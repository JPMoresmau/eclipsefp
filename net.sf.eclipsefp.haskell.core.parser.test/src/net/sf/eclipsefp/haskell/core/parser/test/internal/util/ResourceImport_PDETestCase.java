package net.sf.eclipsefp.haskell.core.parser.test.internal.util;

import java.io.InputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;

import de.leiffrenzel.fp.haskell.core.project.HaskellProjectManager;
import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.test.util.haskell.HaskellProject_PDETestCase;

public class ResourceImport_PDETestCase extends HaskellProject_PDETestCase {

	protected void importSourceFile(final String fileName, final String resFolder, final String resName) throws Exception {
		IHaskellProject hsProject = HaskellProjectManager.get(getProject());
		IPath path = hsProject.getSourcePath().append(fileName);
		IFile newFile = getProject().getFile(path);
		newFile.create(getStream(resFolder, resName, this), true, null);
	}

	private InputStream getStream(final String folder, final String name, final Object context) {
		String resourceName = constructName(folder, name, context);
		ClassLoader classLoader = context.getClass().getClassLoader();
		return classLoader.getResourceAsStream(resourceName);
	}

}
