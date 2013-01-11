/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.eclipse.core.resources.IFile;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * An import to clean
 * @author JP Moresmau
 *
 */
public class ImportClean {
	private Location loc;
	private String text;
	
	public ImportClean(IFile f,JSONObject obj) throws JSONException{
		this.text=obj.getString("t");
		this.loc=new Location(f,obj.getJSONArray("l"));
	}
	
	public Location getLocation() {
		return loc;
	}
	
	public void setLocation(Location loc) {
		this.loc = loc;
	}
	
	public String getText() {
		return text;
	}
	
	public void setText(String text) {
		this.text = text;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return loc.toString()+":"+text;
	}
}
