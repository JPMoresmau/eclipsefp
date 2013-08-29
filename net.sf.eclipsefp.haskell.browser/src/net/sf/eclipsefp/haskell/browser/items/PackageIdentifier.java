/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents the name and version of a package.
 * 
 * @author Alejandro Serrano
 */
public class PackageIdentifier {
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((version == null) ? 0 : version.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		PackageIdentifier other = (PackageIdentifier) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (version == null) {
			if (other.version != null)
				return false;
		} else if (!version.equals(other.version))
			return false;
		return true;
	}

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
