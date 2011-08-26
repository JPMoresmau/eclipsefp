/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a "newtype" declaration.
 * 
 * @author Alejandro Serrano
 */
public class NewType extends Gadt {

	public NewType(String doc, String[] context, String name, String[] vars,
			String kind, Constructor[] cons) {
		this.setDoc(doc);
		this.setType(DeclarationType.NEW_TYPE);
		this.setInfo(context, name, vars, kind, cons);
	}
	
	public NewType(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.setType(DeclarationType.NEW_TYPE);
		this.setInfo(o);
	}
	
	@Override
	public String getCompleteDefinition() {
		return getCompleteDefinition("newtype");
	}	
}
