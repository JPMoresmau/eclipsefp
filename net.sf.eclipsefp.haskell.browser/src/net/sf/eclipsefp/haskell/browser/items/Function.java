package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a function signature.
 * 
 * @author serras
 */

public class Function extends Declaration {
	String signature;

	public Function(String doc, String name, String signature) {
		this.setDoc(doc);
		this.setName(name);
		this.setType(DeclarationType.FUNCTION);
		this.signature = signature;
	}

	public Function(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.setType(DeclarationType.FUNCTION);
		this.setName(o.getString("name"));
		this.signature = o.getString("signature");
	}

	public String getSignature() {
		return this.signature;
	}
}
