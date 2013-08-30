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
	/**
	 * the declaration name
	 */
	private String name;
	/**
	 * the module name
	 */
	private Module module;
	/**
	 * the package name
	 */
	private String packageName;	
	
	public DeclarationId(JSONArray obj)throws JSONException {
		module=new Module(obj.getJSONObject(0));
		name=obj.optString(1);
		packageName=obj.optString(2);
	}
	
	public String getName() {
		return name;
	}
	public Module getModule() {
		return module;
	}
	public String getPackageName() {
		return packageName;
	}
	
	@Override
	public String toString() {
		return name;
	}
	
}
