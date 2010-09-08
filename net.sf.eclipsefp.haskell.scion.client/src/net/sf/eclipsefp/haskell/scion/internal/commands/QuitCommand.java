package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;

public class QuitCommand extends ScionCommand {

	public QuitCommand(IScionCommandRunner runner) {
		super(runner, Job.SHORT);
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		// NOOP

	}

	@Override
	protected String getMethod() {
		return "quit";
	}

}
