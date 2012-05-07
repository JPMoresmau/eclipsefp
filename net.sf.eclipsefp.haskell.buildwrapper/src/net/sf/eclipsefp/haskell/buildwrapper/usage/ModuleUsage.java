/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

/**
 * @author JP Moresmau
 *
 */
public class ModuleUsage {
	private long moduleID;
	private String location;
	private String section;
	
	
	public ModuleUsage(long moduleID, String section,String location) {
		super();
		this.moduleID = moduleID;
		this.section = section;
		this.location = location;
	}
	public long getModuleID() {
		return moduleID;
	}
	public String getLocation() {
		return location;
	}
	/**
	 * @return the section
	 */
	public String getSection() {
		return section;
	}
	
}
