package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

public class Constructor extends Documented {
	String name;
	String signature;
	
	public Constructor(String doc, String name, String signature) {
		this.setDoc(doc);
		this.name = name;
		this.signature = signature;
	}
	
	public Constructor(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.name = o.getString("name");
		this.signature = o.getString("type");
	}
	
	public String getName() {
		return this.name;
	}
	
	public String getSignature() {
		return this.signature;
	}
	
	public String getCompleteDefinition() {
		StringBuilder builder = new StringBuilder(this.getName());
		builder.append(" :: ");
		builder.append(this.getSignature());
		return builder.toString();
	}
}
