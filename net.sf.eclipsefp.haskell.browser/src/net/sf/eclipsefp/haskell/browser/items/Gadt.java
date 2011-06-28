/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
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
		String oKind = o.optString("kind", "");
		if (oKind.equals("null"))
			oKind = "";
		this.setInfo(Util.getStringArray(o.getJSONArray("context")),
				head.getString("name"),
				Util.getStringArray(head.getJSONArray("vars")),
				oKind,
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
	
	public String getCompleteDefinition(String keyword) {
		StringBuilder builder = new StringBuilder(keyword);
		builder.append(' ');
		if (this.getContext().length > 1) {
			builder.append('(');
			builder.append(this.getContext()[0]);
			for (int i = 1; i < this.getContext().length; i++) {
				builder.append(", ");
				builder.append(this.getContext()[i]);
			}
			builder.append(") ");
		} else if (this.getContext().length == 1) {
			builder.append(this.getContext()[0]);
			builder.append(' ');
		}
		if (this.getContext().length > 0)
			builder.append("=> ");
		builder.append(this.getName());
		for (String tvar : this.getTypeVariables()) {
			builder.append(' ');
			builder.append(tvar);
		}
		if (this.getKind().length()>0) {
			builder.append(" :: ");
			builder.append(this.getKind());
		}
		return builder.toString();
	}
	
	@Override
	public String getShownName() {
		StringBuilder name = new StringBuilder(this.name);
		for (String var : this.vars) {
			name.append(' ');
			name.append(var);
		}
		return name.toString();
	}
}
