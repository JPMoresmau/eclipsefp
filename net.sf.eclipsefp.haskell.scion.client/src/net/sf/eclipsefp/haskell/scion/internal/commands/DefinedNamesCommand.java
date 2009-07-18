package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.ScionThreadManager;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;

public class DefinedNamesCommand extends ScionCommand {

	public DefinedNamesCommand(ScionThreadManager manager) {
		super(manager, Job.INTERACTIVE);
	}

	@Override
	protected String getMethod() {
		return "defined-names";
	}

	@Override
	protected void processResult(Object result) throws JSONException {
		// TODO
	}

}
