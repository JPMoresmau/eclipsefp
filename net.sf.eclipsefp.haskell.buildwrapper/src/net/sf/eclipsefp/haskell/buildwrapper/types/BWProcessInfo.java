/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;

/**
 * Info about a single BuildWrapper long running process
 * @author JP Moresmau
 *
 */
public class BWProcessInfo {
	private Process process;
	private IFile currentFile;
	private Set<IFile> files=new HashSet<IFile>();
	
	public BWProcessInfo(Process process, IFile currentFile) {
		super();
		this.process = process;
		this.currentFile = currentFile;
		this.files.add(currentFile);
	}

	public IFile getCurrentFile() {
		return currentFile;
	}

	public void setCurrentFile(IFile currentFile) {
		this.currentFile = currentFile;
	}

	public Process getProcess() {
		return process;
	}

	/**
	 * @return the files
	 */
	public Set<IFile> getFiles() {
		return files;
	}

}
