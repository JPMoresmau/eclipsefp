package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;
import org.json.JSONObject;

public class ConnectionInfoCommand extends ScionCommand {
	
	private int pid;
	private int version;

	public ConnectionInfoCommand(IScionCommandRunner runner) {
		super(runner, Job.SHORT);
	}

	@Override
	protected String getMethod() {
		return "connection-info";
	}

	@Override
	protected void doProcessResult(Object obj) throws JSONException {
		JSONObject result = (JSONObject)obj;
		pid = result.getInt("pid");
		version = result.getInt("version");
	}
	
	public int getPid() {
		return pid;
	}
	
	public int getVersion() {
		return version;
	}

}
