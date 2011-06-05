package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents an "instance" declaration.
 * 
 * @author serras
 */

public class Instance extends Declaration {
	String[] context;
	String[] vars;

	public Instance(String doc, String[] context, String name, String[] vars) {
		this.setDoc(doc);
		this.setType(DeclarationType.INSTANCE);
		this.setInfo(context, name, vars);
	}

	public Instance(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.setType(DeclarationType.INSTANCE);
		this.setInfo(o);
	}

	protected void setInfo(String[] context, String name, String[] vars) {
		this.context = context;
		this.setName(name);
		this.vars = vars;
	}

	protected void setInfo(JSONObject o) throws JSONException {
		this.setDoc(o);

		JSONObject head = o.getJSONObject("head");
		this.setInfo(Util.getStringArray(o.getJSONArray("context")),
				head.getString("name"),
				Util.getStringArray(head.getJSONArray("vars")));
	}

	public String[] getContext() {
		return this.context;
	}

	public String[] getTypeVariables() {
		return this.vars;
	}
}
