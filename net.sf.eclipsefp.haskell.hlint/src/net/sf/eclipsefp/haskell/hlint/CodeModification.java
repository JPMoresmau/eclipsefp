/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

import java.io.Serializable;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Base class for all kinds of suggestions that involve
 * modification of the Haskell source code.
 * @author Alejandro Serrano
 *
 */
public abstract class CodeModification implements Serializable {
	private CodeModificationType type;
	
	public CodeModificationType getType() {
		return this.type;
	}
	
	protected void setType(CodeModificationType type) {
		this.type = type;
	}
	
	public abstract Object toJSON() throws JSONException;
	
	public static CodeModification fromJSON(Object o){
		if (CodeModificationType.REMOVE.toString().equals(o)){
			return new CodeModificationRemove();
		} else if (o instanceof JSONObject){
			JSONObject obj=(JSONObject)o;
			String t=obj.optString(CodeModificationType.TEXT.toString());
			if (t!=null && t.length()>0){
				return new CodeModificationText(t);
			}
		}
		return null;
	}
}
