package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;

public class DefinedNamesCommand extends ScionCommand {

	public DefinedNamesCommand(IScionCommandRunner runner) {
		super(runner, Job.INTERACTIVE);
	}

	@Override
	protected String getMethod() {
		return "defined-names";
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		// TODO
	}

}
