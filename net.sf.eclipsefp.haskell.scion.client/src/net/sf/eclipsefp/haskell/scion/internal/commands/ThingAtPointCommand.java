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
	
	private boolean qualify;
	private boolean typed;
	
	public ThingAtPointCommand(Location location,boolean qualify,boolean typed) {
		super();
		this.location = location;
		this.qualify=qualify;
		this.typed=typed;
	}
	
	
	
	public String getThing() {
		return thing;
	}

	@Override
  public String getMethod() {
		return "thing-at-point";
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("file", location.getFileName());
		params.put("line", location.getStartLine() );
		params.put("column", location.getStartColumn());
		params.put("qualify",qualify);
		params.put("typed",typed);
		return params;
	}

	@Override
	protected void doProcessResult() throws JSONException {
		if (response instanceof String) {
			thing = (String) response;
		} else if (!JSONObject.NULL.equals(response)){
			JSONObject result = (JSONObject) response;
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



	public boolean isQualify() {
		return qualify;
	}



	public void setQualify(boolean qualify) {
		this.qualify = qualify;
	}



	public boolean isTyped() {
		return typed;
	}



	public void setTyped(boolean typed) {
		this.typed = typed;
	}

}
