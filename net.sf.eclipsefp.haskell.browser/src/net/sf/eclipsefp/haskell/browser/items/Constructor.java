package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

public class Constructor extends Documented {
	String name;
	String type;
	
	public Constructor(String doc, String name, String type) {
		this.setDoc(doc);
		this.name = name;
		this.type = type;
	}
	
	public Constructor(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.name = o.getString("name");
		this.type = o.getString("type");
	}
	
	public String getName() {
		return this.name;
	}
	
	public String getType() {
		return this.type;
	}
}
