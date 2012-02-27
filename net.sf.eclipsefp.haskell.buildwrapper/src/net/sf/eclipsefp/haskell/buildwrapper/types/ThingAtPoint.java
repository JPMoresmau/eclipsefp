/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * @author JP Moresmau
 *
 */
public class ThingAtPoint {
	private String name;
	private String module;
	private String type;
	private String qualifiedType;
	private String haddockType;
	private String ghcType;
	
	public ThingAtPoint(JSONObject obj) throws JSONException{
		name=obj.getString("Name");
		module=getString(obj,"Module");
		type=getString(obj, "Type");
		qualifiedType=getString(obj, "QType");
		haddockType=getString(obj, "HType");
		ghcType=getString(obj, "GType");
	}
	
	public ThingAtPoint(String name,String haddockType) {
		this.name=name;
		this.haddockType=haddockType;
	}
	private String getString(JSONObject obj,String name){
		String s=obj.optString(name);
		if ("".equals(s) || "null".equals(s)){
			s=null;
		}
		return s;
	}
	
	public String getName() {
		return name;
	}
	public String getModule() {
		return module;
	}
	public String getType() {
		return type;
	}
	public String getQualifiedType() {
		return qualifiedType;
	}
	public String getHaddockType() {
		return haddockType;
	}
	public String getGhcType() {
		return ghcType;
	}
	
	
}
