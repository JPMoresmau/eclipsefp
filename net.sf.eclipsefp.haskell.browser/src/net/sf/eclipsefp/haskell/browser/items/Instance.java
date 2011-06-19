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
		this.setInfo(Util.getStringArray(o.getJSONArray("context")), head.getString("name"),
				Util.getStringArray(head.getJSONArray("vars")));
	}

	public String[] getContext() {
		return this.context;
	}

	public String[] getTypeVariables() {
		return this.vars;
	}

	@Override
	public String getCompleteDefinition() {
		StringBuilder builder = new StringBuilder("instance");
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
		return builder.toString();
	}

	@Override
	public String getShownName() {
		StringBuilder name = new StringBuilder();

		if (this.context.length > 1) {
			name.append('(');
			name.append(this.context[0]);
			for (int i = 1; i < this.context.length; i++) {
				name.append(", ");
				name.append(this.context[i]);
			}
			name.append(") ");
		} else if (this.context.length == 1) {
			name.append(this.context[0]);
			name.append(' ');
		}
		if (this.context.length > 0) {
			name.append("=> ");
		}
		name.append(this.name);
		for (String var : this.vars) {
			name.append(' ');
			name.append(var);
		}
		return name.toString();
	}
}
