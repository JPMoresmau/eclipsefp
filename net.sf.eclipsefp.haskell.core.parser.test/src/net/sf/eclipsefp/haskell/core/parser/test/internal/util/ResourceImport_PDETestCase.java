package net.sf.eclipsefp.haskell.core.parser.test.internal.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;

import de.leiffrenzel.fp.haskell.core.project.HaskellProjectManager;
import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.parser.HaskellParser_PDETest;
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

	public String getModuleContents(String resKey) throws IOException {
		BufferedReader is = new BufferedReader( new InputStreamReader(getStream(resKey, "Main.hs", new HaskellParser_PDETest())));
		StringBuffer buf = new StringBuffer();
		String line = is.readLine();
		while(line != null) {
			buf.append(line);
			buf.append('\n');
			line = is.readLine();
		}
		return buf.toString();
	}

}
