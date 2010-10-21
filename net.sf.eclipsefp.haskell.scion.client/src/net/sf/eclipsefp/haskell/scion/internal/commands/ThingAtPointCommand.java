package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.types.Location;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Command that returns information about the thing under the "point"
 * (which is Emacs-speak for "cursor").
 * 
 * @author Thomas ten Cate
 */
public class ThingAtPointCommand extends ScionCommand {

	private Location location;
	
	private String thing; // the response
	
	public ThingAtPointCommand(Location location) {
		super();
		this.location = location;
	}
	
	public String getThing() {
		return thing;
	}

	@Override
	protected String getMethod() {
		return "thing-at-point";
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("file", location.getFileName());
		params.put("line", location.getStartLine() );
		params.put("column", location.getStartColumn());
		return params;
	}

	@Override
	protected void doProcessResult(Object json) throws JSONException {
		if (json instanceof String){
			thing=(String)json;
		} else if (!JSONObject.NULL.equals(json)){
			JSONObject result = (JSONObject)json;
			if (result.has("Just")) {
				thing = result.getString("Just");
			} else {
				thing = null;
			}
		}
		if ("no info".equalsIgnoreCase(thing)) {
			thing = null; // ... yeah
		}
	}

}
