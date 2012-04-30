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
	
	
	
	public ModuleUsage(long moduleID, String location) {
		super();
		this.moduleID = moduleID;
		this.location = location;
	}
	public long getModuleID() {
		return moduleID;
	}
	public String getLocation() {
		return location;
	}
		
	
}
