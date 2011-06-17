package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents the name and version of a package.
 * 
 * @author serras
 */

public class PackageIdentifier {
	String name;
	String version;

	public PackageIdentifier(String name, String version) {
		this.name = name;
		this.version = version;
	}

	public PackageIdentifier(JSONObject o) throws JSONException {
		this.name = o.getString("name");
		this.version = o.getString("version");
	}

	public String getName() {
		return this.name;
	}

	public String getVersion() {
		return this.version;
	}
	
	public JSONObject toJSON() throws JSONException {
		JSONObject o = new JSONObject();
		o.put("name", this.name);
		o.put("version", this.version);
		return o;
	}
	
	public String toString() {
		return this.name + "-" + this.version;
	}
}
