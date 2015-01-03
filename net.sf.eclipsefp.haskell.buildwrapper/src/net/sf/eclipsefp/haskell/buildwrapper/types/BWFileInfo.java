/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.eclipse.core.resources.IFile;

/**
 * Info about a single file
 * @author JP Moresmau
 *
 */
public class BWFileInfo {
	private IFile file;
	/**
	 * the cabal stanza or component we analyse the file in
	 */
	private String stanza;
	/**
	 * 
	 */
	public BWFileInfo(IFile file,String stanza) {
		this.file=file;
		this.stanza=stanza;
	}
	
	/**
	 * @return the file
	 */
	public IFile getFile() {
		return file;
	}
	
	/**
	 * @return the stanza
	 */
	public String getStanza() {
		return stanza;
	}

}
