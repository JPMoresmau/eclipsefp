package net.sf.eclipsefp.haskell.scion.commands;

import org.json.JSONException;
import org.json.JSONObject;

public class SetVerbosityCommand extends ScionCommand {

	private int verbosity;
	
	public SetVerbosityCommand(int verbosity) {
		this.verbosity = verbosity;
	}
	
	@Override
	protected String getMethod() {
		return "set-verbosity";
	}
	
	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("level", verbosity);
		return params;
	}

	@Override
	protected void processResult(Object result) throws JSONException {
		// nothing interesting to do
	}

}
