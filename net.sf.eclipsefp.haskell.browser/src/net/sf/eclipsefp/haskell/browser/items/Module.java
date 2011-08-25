/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Item representing a Haskell module.
 * 
 * @author Alejandro Serrano
 */
public class Module extends Documented {
	String name;

	public Module(String doc, String name) {
		this.setDoc(doc);
		this.name = name;
	}
	
	public Module(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.name = o.getString("name");
	}
	
	public String getName() {
		return this.name;
	}
	
	@Override
	public String getCompleteDefinition() {
		return "module " + name;
	}
}
