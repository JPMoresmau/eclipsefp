package net.sf.eclipsefp.haskell.scion.types;

import java.util.Comparator;

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
	
	public static Comparator<OutlineDef> BY_LOCATION=new Comparator<OutlineDef>() {
		public int compare(OutlineDef o1, OutlineDef o2) {
			Location l1=o1.getLocation();
			Location l2=o2.getLocation();
			if (l1.getStartLine()==l2.getStartLine()){
				return l1.getStartColumn()-l2.getStartColumn();
			} else {
				return l1.getStartLine()-l2.getStartLine();
			}
		}
	};
	
	public static Comparator<OutlineDef> BY_NAME=new Comparator<OutlineDef>() {
		public int compare(OutlineDef o1, OutlineDef o2) {
			return o1.getName().compareToIgnoreCase(o2.getName());
		}
	};
	
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
