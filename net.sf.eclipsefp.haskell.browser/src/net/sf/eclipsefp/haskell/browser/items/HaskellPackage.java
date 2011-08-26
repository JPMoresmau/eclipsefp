/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Item representing a Hackage package.
 * 
 * @author Alejandro Serrano
 */
public class HaskellPackage extends Documented {
	PackageIdentifier id;

	public HaskellPackage(String doc, PackageIdentifier id) {
		this.setDoc(doc);
		this.id = id;
	}

	public HaskellPackage(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.id = new PackageIdentifier(o.getJSONObject("id"));
	}

	public PackageIdentifier getIdentifier() {
		return this.id;
	}
	
	@Override
	public String getCompleteDefinition() {
		return "package " + id.toString();
	}
	
	@Override
	public String getName() {
		return id.toString();
	}
}
