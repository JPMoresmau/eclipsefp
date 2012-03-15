/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.LinkedList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Build flags used for a particular file, as calculated from Cabal
 * @author JP Moresmau
 *
 */
public class BuildFlags {
	List<String> ghcFlags=new LinkedList<String>();
	
	public BuildFlags(JSONObject obj){
		JSONArray arr=obj.optJSONArray("a");
		if (arr!=null){
			for (int a=0;a<arr.length();a++){
				String s=arr.optString(a);
				if (s!=null && s.length()>0){
					ghcFlags.add(s);
				}
			}
		}
	}
	
	/**
	 * @return the ghcFlags
	 */
	public List<String> getGhcFlags() {
		return ghcFlags;
	}
}
