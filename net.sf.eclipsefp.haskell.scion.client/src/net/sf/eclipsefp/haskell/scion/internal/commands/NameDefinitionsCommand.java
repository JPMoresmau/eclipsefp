package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.Location;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Command that lists the places where a given identifier is defined.
 * 
 * @author Thomas ten Cate
 */
public class NameDefinitionsCommand extends ScionCommand {

	private String name;
	
	private Location[] locations;

	public NameDefinitionsCommand(IScionCommandRunner runner, String name) {
		super(runner, Job.INTERACTIVE);
		this.name = name;
	}
	
	@Override
	protected String getMethod() {
		return "name-definitions";
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("name", name);
		return params;
	}

	@Override
	protected void doProcessResult(Object json) throws JSONException {
		JSONArray result = (JSONArray)json;
		locations = new Location[result.length()];
		for (int i = 0; i < locations.length; ++i) {
			locations[i] = new Location(result.getJSONObject(i));
		}
	}

	public boolean isFound() {
		return locations != null && locations.length > 0;
	}
	
	public Location[] getLocations(int index) {
		return locations;
	}
	
	public Location getFirstLocation() {
		return locations[0];
	}

}
