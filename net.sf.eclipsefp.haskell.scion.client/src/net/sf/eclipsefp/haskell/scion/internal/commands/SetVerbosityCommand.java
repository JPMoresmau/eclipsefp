package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.json.JSONException;
import org.json.JSONObject;

public class SetVerbosityCommand extends ScionCommand {

	private int verbosity;
	
	public SetVerbosityCommand(int verbosity) {
		super();
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
	protected void doProcessResult() throws JSONException {
		// nothing interesting to do
	}

}
