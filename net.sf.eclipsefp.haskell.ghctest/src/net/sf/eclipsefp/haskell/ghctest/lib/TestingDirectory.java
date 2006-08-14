/* *****************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 * *****************************************************************************/
package net.sf.eclipsefp.haskell.ghctest.lib;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;

public class TestingDirectory {

	private File fPath;

	public TestingDirectory() {
		int dir = 1;
		while((fPath = createTempPathname(dir)).exists()) {
			++dir;
		}
		fPath.mkdir();
	}

	private File createTempPathname(int dir) {
		return new File(System.getProperty("java.io.tmpdir"),
		         Integer.toString(dir));
	}
	
	public File getPathname() {
		return fPath;
	}

	public void destroy() {
		destroy(fPath);
	}

	public File createFile(String fileName, String contents) throws IOException {
		File file = new File(fPath, fileName);
		Writer writer = new FileWriter(file);
		writer.write(contents);
		writer.close();
		return file;
	}
	
	private void destroy(File path) {
		if (path.isDirectory()) {
			for(File child : path.listFiles()) {
				destroy(child);
			}
		}

		path.delete();
	}

}
