package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a "type" declaration.
 * 
 * @author serras
 */

public class TypeSynonym extends Declaration {
	String[] vars;
	String equals;

	public TypeSynonym(String doc, String name,
			String[] vars, String equals) {
		this.setDoc(doc);
		this.setType(DeclarationType.TYPE_SYNONYM);
		this.setInfo(name, vars, equals);
	}

	public TypeSynonym(JSONObject o) throws JSONException {
		this.setDoc(o);
		this.setType(DeclarationType.TYPE_SYNONYM);
		this.setInfo(o);
	}

	protected void setInfo(String name, String[] vars,
			String equals) {
		this.setName(name);
		this.vars = vars;
		this.equals = equals;
	}

	protected void setInfo(JSONObject o) throws JSONException {
		this.setDoc(o);

		JSONObject head = o.getJSONObject("head");
		this.setInfo(head.getString("name"),
				Util.getStringArray(head.getJSONArray("vars")),
				o.getString("equals"));
	}

	public String[] getTypeVariables() {
		return this.vars;
	}

	public String getEquivalence() {
		return this.equals;
	}
}
