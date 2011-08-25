/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a "type" declaration.
 * 
 * @author Alejandro Serrano
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
	
	@Override
	public String getCompleteDefinition() {
		StringBuilder builder = new StringBuilder("type");
		builder.append(' ');
		builder.append(this.getName());
		for (String tvar : this.getTypeVariables()) {
			builder.append(' ');
			builder.append(tvar);
		}
		builder.append(" = ");
		builder.append(this.getEquivalence());
		return builder.toString();
	}
	
	@Override
	public String getShownName() {
		StringBuilder name = new StringBuilder(this.name);
		for (String var : this.vars) {
			name.append(' ');
			name.append(var);
		}
		name.append(" = ");
		name.append(this.equals);
		return name.toString();
	}
}
