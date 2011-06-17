package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Item representing a Hackage package.
 * 
 * @author serras
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
}
