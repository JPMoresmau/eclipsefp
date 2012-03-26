/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONArray;
import org.json.JSONException;

/**
 * Wraps a declaration name (or "") and a module. Used to indicate the type name when searching where a constructor comes from
 * @author JP Moresmau
 *
 */
public class DeclarationId {
	private String name;
	private Module module;
		
	
	public DeclarationId(JSONArray obj)throws JSONException {
		module=new Module(obj.getJSONObject(0));
		name=obj.optString(1);
	}
	
	public String getName() {
		return name;
	}
	public Module getModule() {
		return module;
	}
	
	
}
