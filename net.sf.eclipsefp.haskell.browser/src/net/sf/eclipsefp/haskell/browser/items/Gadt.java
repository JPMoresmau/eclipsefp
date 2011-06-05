package net.sf.eclipsefp.haskell.browser.items;

import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a GADT-like declaration, that is, a data or a newtype declaration.
 * 
 * @author serras
 */

public abstract class Gadt extends Declaration {
	String[] context;
	String[] vars;
	String kind;
	Constructor[] cons;

	protected void setInfo(String[] context, String name, String[] vars,
			String kind, Constructor[] cons) {
		this.context = context;
		this.setName(name);
		this.vars = vars;
		this.kind = kind;
		this.cons = cons;
	}

	protected void setInfo(JSONObject o) throws JSONException {
		this.setDoc(o);

		ArrayList<Constructor> aCons = new ArrayList<Constructor>();
		JSONArray jCons = o.getJSONArray("decls");
		for (int i = 0; i < jCons.length(); i++) {
			aCons.add(new Constructor(jCons.getJSONObject(i)));
		}

		JSONObject head = o.getJSONObject("head");
		this.setInfo(Util.getStringArray(o.getJSONArray("context")),
				head.getString("name"),
				Util.getStringArray(head.getJSONArray("vars")),
				o.optString("kind", ""),
				aCons.toArray(new Constructor[jCons.length()]));
	}

	public String[] getContext() {
		return this.context;
	}

	public String[] getTypeVariables() {
		return this.vars;
	}

	public String getKind() {
		return this.kind;
	}

	public Constructor[] getConstructors() {
		return this.cons;
	}
}
