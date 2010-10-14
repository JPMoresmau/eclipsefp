package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;

import org.json.JSONException;

public class ArbitraryCommand extends ScionCommand {

	public ArbitraryCommand(IScionCommandRunner runner, IScionServer server, int priority) {
		super(runner, server, priority);
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
