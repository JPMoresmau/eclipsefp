package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.ScionThreadManager;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;

public class ConnectionInfoCommand extends ScionCommand {

	public ConnectionInfoCommand(ScionThreadManager manager) {
		super(manager, Job.SHORT);
	}

	@Override
	protected String getMethod() {
		return "connection-info";
	}

	@Override
	protected void processResult(Object result) throws JSONException {
		System.err.println(result);
	}

}
