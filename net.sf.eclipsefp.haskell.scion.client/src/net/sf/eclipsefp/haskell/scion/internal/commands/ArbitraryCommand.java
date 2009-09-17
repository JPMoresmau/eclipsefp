package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

import org.json.JSONException;

public class ArbitraryCommand extends ScionCommand {

	public ArbitraryCommand(IScionCommandRunner runner, int priority) {
		super(runner, priority);
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		throw new IllegalStateException();

	}

	@Override
	protected String getMethod() {
		return "";
	}

}
