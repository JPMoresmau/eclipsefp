package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;
import org.json.JSONObject;

public class SetVerbosityCommand extends ScionCommand {

	private int verbosity;
	
	public SetVerbosityCommand(IScionCommandRunner runner, IScionServer server, int verbosity) {
		super(runner, server, Job.SHORT);
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
	protected void doProcessResult(Object result) throws JSONException {
		// nothing interesting to do
	}

}
