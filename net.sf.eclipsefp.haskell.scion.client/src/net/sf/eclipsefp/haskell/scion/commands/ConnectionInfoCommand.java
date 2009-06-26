package net.sf.eclipsefp.haskell.scion.commands;

import org.json.JSONException;


public class ConnectionInfoCommand extends ScionCommand {

	@Override
	protected String getMethod() {
		return "connection-info";
	}

	@Override
	protected void processResult(Object result) throws JSONException {
		System.err.println(result);
	}

}
