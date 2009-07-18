package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.ScionThreadManager;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;
import org.json.JSONObject;

public class SetVerbosityCommand extends ScionCommand {

	private int verbosity;
	
	public SetVerbosityCommand(ScionThreadManager manager, int verbosity) {
		super(manager, Job.SHORT);
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
