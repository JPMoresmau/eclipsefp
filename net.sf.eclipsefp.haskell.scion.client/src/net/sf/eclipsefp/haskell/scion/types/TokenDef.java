package net.sf.eclipsefp.haskell.scion.types;

import org.json.JSONArray;
import org.json.JSONException;

/**
 * 
 * @author JP Moresmau
 *
 */
public class TokenDef {
	private Location location;
	private String name;
	
	public TokenDef(JSONArray json) throws JSONException {
		this.name = json.getString(0);
		this.location = new Location("",json.getInt(1),json.getInt(2),json.getInt(3),json.getInt(4));
	}
	
	public Location getLocation() {
		return location;
	}
	
	public String getName() {
		return name;
	}
}
