/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.eclipse.core.resources.IFile;
import org.json.JSONArray;
import org.json.JSONException;

/**
 * @author JP Moresmau
 *
 */
public class SearchResultLocation extends Location {
	private IFile ifile;
	private boolean definition;
	
	public SearchResultLocation(IFile f, JSONArray json) throws JSONException {
		super(f,json);
		this.ifile=f;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.buildwrapper.types.Location#getIFile()
	 */
	public IFile getIFile() {
		return ifile;
	}
	
	/**
	 * @param ifile the ifile to set
	 */
	public void setIFile(IFile ifile) {
		this.ifile = ifile;
	}

	public boolean isDefinition() {
		return definition;
	}

	public void setDefinition(boolean definition) {
		this.definition = definition;
	}
}
