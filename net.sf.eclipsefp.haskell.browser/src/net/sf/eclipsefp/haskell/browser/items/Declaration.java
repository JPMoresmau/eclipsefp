/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.json.JSONArray;
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

	public static Collection<Declaration> fromJSON(JSONObject o) throws Exception {
		String type = o.getString("type");

		if (type.equals("data"))
			return Collections.<Declaration>singleton(new DataType(o));
		else if (type.equals("newtype"))
			return Collections.<Declaration>singleton(new NewType(o));
		else if (type.equals("class"))
			return Collections.<Declaration>singleton(new TypeClass(o));
		else if (type.equals("instance"))
			return Collections.<Declaration>singleton(new Instance(o));
		else if (type.equals("signature")){
			Collection<Declaration> ret = new ArrayList<Declaration>();
			JSONArray arr = o.optJSONArray("name");
			if (arr != null){
				for (int a = 0; a < arr.length(); a++){
					ret.add(new Function(arr.getString(a), o));
				}
			} else {
				String name = o.getString("name");
				ret.add(new Function(name, o));
			}
			return ret;
		} else if (type.equals("type"))
			return Collections.<Declaration>singleton(new TypeSynonym(o));
		else
			throw new Exception("Declaration of unknown type");
	}
	
	public abstract String getShownName();
}
