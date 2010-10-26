package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.json.JSONException;
import org.json.JSONObject;

public class ConnectionInfoCommand extends ScionCommand {
	
	private int pid;
	private String version;

	public ConnectionInfoCommand() {
		super();
	}

	@Override
	protected String getMethod() {
		return "connection-info";
	}

	@Override
	protected void doProcessResult() throws JSONException {
		JSONObject result = (JSONObject) response;
		pid = result.getInt("pid");
		version = result.getString("version");
	}
	
	public int getPid() {
		return pid;
	}
	
	public String getVersion() {
		return version;
	}

}
