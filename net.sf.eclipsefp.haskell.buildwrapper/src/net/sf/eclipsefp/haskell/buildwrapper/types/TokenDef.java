package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.eclipse.core.resources.IFile;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * 
 * @author JP Moresmau
 *
 */
public class TokenDef {
	private Location location;
	private String name;
	
	public TokenDef(String name) {
		this.name = name;
		this.location = new Location("", 0, 0, 0, 0);
	}
	
	public TokenDef(String name, Location location) {
		this.name = name;
		this.location = location;
	}
	
	public TokenDef(JSONArray json) throws JSONException {
		this.name = json.getString(0);
		this.location = new Location("",json.getInt(1),json.getInt(2),json.getInt(3),json.getInt(4));
	}
	
	public TokenDef(IFile f,JSONObject obj) throws JSONException{
		this.name=JSONObject.getNames(obj)[0];
		this.location=new Location(f,obj.getJSONArray(name));
	}
	
	public Location getLocation() {
		return location;
	}
	
	public String getName() {
		return name;
	}
	
	public void move(int line, int column) {
		boolean allInSameLine = location.getStartLine() == location.getEndLine();
		this.location = new Location("", location.getStartLine() + line, location.getStartColumn() + column,
				location.getEndLine() + line, location.getEndColumn() + (allInSameLine ? column : 0));
	}
}
