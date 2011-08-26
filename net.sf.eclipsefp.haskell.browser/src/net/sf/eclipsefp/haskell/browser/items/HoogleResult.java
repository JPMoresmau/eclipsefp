/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

import org.json.JSONObject;

/**
 * Represents information returned by a Hoogle search.
 * 
 * @author Alejandro Serrano
 */
public abstract class HoogleResult {
	private HoogleResultType type;
	
	public HoogleResultType getType() {
		return this.type;
	}
	
	protected void setType(HoogleResultType type) {
		this.type = type;
	}
	
	public abstract String getName();
	
	public abstract String getCompleteDefinition();
	
	public static HoogleResult fromJSON(JSONObject o) throws Exception {
		String type = o.getString("type");
		if (type.equals("package"))
			return new HoogleResultPackage(o);
		else if (type.equals("module"))
			return new HoogleResultModule(o);
		else if (type.equals("declaration"))
			return new HoogleResultDeclaration(o);
		else if (type.equals("constructor"))
			return new HoogleResultConstructor(o);
		else
			return null;
	}
}
