package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a "class" declaration.
 * 
 * @author serras
 */

public class TypeClass extends Declaration {
	String[] context;
	String[] vars;
	String[] fundeps;

	public TypeClass(String doc, String[] context, String name, String[] vars,
			String[] fundeps) {
		this.setDoc(doc);
		this.setType(DeclarationType.TYPE_CLASS);
		this.setInfo(context, name, vars, fundeps);
	}

	public TypeClass(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.setType(DeclarationType.TYPE_CLASS);
		this.setInfo(o);
	}

	protected void setInfo(String[] context, String name, String[] vars,
			String[] fundeps) {
		this.context = context;
		this.setName(name);
		this.vars = vars;
		this.fundeps = fundeps;
	}

	protected void setInfo(JSONObject o) throws JSONException {
		this.setDoc(o);

		JSONObject head = o.getJSONObject("head");
		this.setInfo(Util.getStringArray(o.getJSONArray("context")),
				head.getString("name"),
				Util.getStringArray(head.getJSONArray("vars")),
				Util.getStringArray(o.getJSONArray("fundeps")));
	}

	public String[] getContext() {
		return this.context;
	}

	public String[] getTypeVariables() {
		return this.vars;
	}

	public String[] getFunctionalDependencies() {
		return this.fundeps;
	}
}
