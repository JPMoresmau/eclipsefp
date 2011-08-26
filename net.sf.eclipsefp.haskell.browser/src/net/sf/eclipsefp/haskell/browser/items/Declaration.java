/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONObject;

/**
 * Represents one of the top level declarations in a module.
 * 
 * @author Alejandro Serrano
 */
public abstract class Declaration extends Documented {
	DeclarationType type;
	String name;

	public DeclarationType getType() {
		return this.type;
	}

	protected void setType(DeclarationType type) {
		this.type = type;
	}

	public String getName() {
		return this.name;
	}

	protected void setName(String name) {
		this.name = name;
	}

	public static Declaration fromJSON(JSONObject o) throws Exception {
		String type = o.getString("type");

		if (type.equals("data"))
			return new DataType(o);
		else if (type.equals("newtype"))
			return new NewType(o);
		else if (type.equals("class"))
			return new TypeClass(o);
		else if (type.equals("instance"))
			return new Instance(o);
		else if (type.equals("signature"))
			return new Function(o);
		else if (type.equals("type"))
			return new TypeSynonym(o);
		else
			throw new Exception("Declaration of unknown type");
	}
	
	public abstract String getShownName();
}
