package net.sf.eclipsefp.haskell.scion.types;

import org.json.JSONException;
import org.json.JSONObject;

public class Note {

	public enum Kind { ERROR, WARNING, INFO, OTHER };
	
	private Kind kind; // error, warning, info or other
	private Location location;
	private String message;
	
	public Note(JSONObject json) throws JSONException {
		String kind = json.getString("kind");
		if (kind.equals("error"))
			this.kind = Kind.ERROR;
		else if (kind.equals("warning"))
			this.kind = Kind.WARNING;
		else if (kind.equals("info"))
			this.kind = Kind.INFO;
		else
			this.kind = Kind.OTHER;
		
		this.location = new Location(json.getJSONObject("location"));
		
		this.message = json.getString("message");
	}
	
	public Kind getKind() {
		return kind;
	}
	
	public Location getLocation() {
		return location;
	}
	
	public String getMessage() {
		return message;
	}
	
	@Override
	public String toString() {
		return String.format("%s:%s: %s", kind.toString(), location.toString(), message);
	}
	
}
