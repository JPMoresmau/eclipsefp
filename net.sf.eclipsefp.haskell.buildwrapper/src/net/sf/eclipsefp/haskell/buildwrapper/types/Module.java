/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

/**
 * @author JP Moresmau
 *
 */
public class Module {
	private String moduleName;
	private String packageName;
	private long moduleID;
	private Long fileID;
	
	
	public Module(long moduleID, String packageName, String moduleName,Long fileID) {
		super();
		this.moduleID = moduleID;
		this.packageName = packageName;
		this.moduleName = moduleName;
		this.fileID=fileID;
	}
	
	public String getModuleName() {
		return moduleName;
	}

	public String getPackageName() {
		return packageName;
	}

	public long getModuleID() {
		return moduleID;
	}
	public Long getFileID() {
		return fileID;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (int) (moduleID ^ (moduleID >>> 32));
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Module other = (Module) obj;
		if (moduleID != other.moduleID)
			return false;
		return true;
	}


	
	
}
