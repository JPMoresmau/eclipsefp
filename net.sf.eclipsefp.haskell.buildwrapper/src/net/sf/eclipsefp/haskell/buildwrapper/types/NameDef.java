/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import net.sf.eclipsefp.haskell.buildwrapper.types.OutlineDef.OutlineDefType;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A name def represents a GHC Name
 * the difference with outline is mainly we don't have source locations
 * @author JP Moresmau
 *
 */
public class NameDef {
	private Set<OutlineDefType> types;
	private String name;
	
	/**
	 * the type signature or "?" if not known
	 */
	private String typeSignature;
	
	public NameDef(JSONObject obj) throws JSONException{
		this.name=obj.getString("n");
		// remove module
		int ix=name.lastIndexOf('.');
		if (ix>-1){
			name=name.substring(ix+1);
		}
		JSONArray arr=obj.getJSONArray("t");
		types=new HashSet<>(arr.length());
		for (int a=0;a<arr.length();a++){
			types.add(OutlineDefType.valueOf(arr.getString(a).toUpperCase(Locale.ENGLISH)));
		}
		typeSignature=obj.optString("s");
		if (typeSignature==null || typeSignature.length()==0 || "null".equals(typeSignature)){
			typeSignature="?";
		}
	}
	
	public String getName() {
		return name;
	}
	
	public Set<OutlineDefType> getTypes() {
		return types;
	}
	
	/**
	 * @return the typeSignature
	 */
	public String getTypeSignature() {
		return typeSignature;
	}
}
