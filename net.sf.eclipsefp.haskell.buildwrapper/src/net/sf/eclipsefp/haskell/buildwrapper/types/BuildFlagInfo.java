/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.json.JSONArray;

/**
 * Keeps build flags and notes for file
 * @author JP Moresmau
 *
 */
public class BuildFlagInfo {
	private String flags;
	private JSONArray notes;
	
	
	
	public BuildFlagInfo(String flags, JSONArray notes) {
		super();
		this.flags = flags;
		this.notes = notes;
	}
	
	public String getFlags() {
		return flags;
	}
	public JSONArray getNotes() {
		return notes;
	}
	
	
}
