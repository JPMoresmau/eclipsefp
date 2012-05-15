/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.usage;

/**
 * Simple structure for recording where an object is used
 * @author JP Moresmau
 *
 */
public class ObjectUsage {
	/**
	 * the object id in the db
	 */
	private long objectID;
	/**
	 * location string
	 */
	private String location;
	/**
	 * section name
	 */
	private String section;
	
	
	public ObjectUsage(long moduleID, String section,String location) {
		super();
		this.objectID = moduleID;
		this.section = section;
		this.location = location;
	}
	public long getObjectID() {
		return objectID;
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
