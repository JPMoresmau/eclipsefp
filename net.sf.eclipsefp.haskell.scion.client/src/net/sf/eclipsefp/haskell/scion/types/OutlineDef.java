package net.sf.eclipsefp.haskell.scion.types;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Structure sent back from outline
 * @author JP Moresmau
 *
 */
public class OutlineDef {
	public enum OutlineDefType {
		CLASS,
		DATA,
		FAMILY,
		FUNCTION,
		PATTERN,
		SYN,
		TYPE
	}
	
	private OutlineDefType type=OutlineDefType.TYPE;
	private String name;
	private Location loc;
	
	public OutlineDef(String name, OutlineDefType type, Location loc) {
		super();
		this.name = name;
		this.type = type;
		this.loc = loc;
	}
	
	public OutlineDef(JSONObject obj) throws JSONException{
		this.name=obj.getString("name");
		String sType=obj.optString("type");
		if (sType!=null){
			try {
				type=OutlineDefType.valueOf(sType.toUpperCase());
			} catch (IllegalArgumentException iae){
				ScionPlugin.logWarning(sType+" is not a valid outlinedef type", iae);
			}
		}
		this.loc=new Location(obj.getJSONObject("location"));
	}

	public OutlineDefType getType() {
		return type;
	}

	public String getName() {
		return name;
	}

	public Location getLocation() {
		return loc;
	}
	
	
	@Override
	public String toString() {
		return getName();
	}
	
}
