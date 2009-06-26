package net.sf.eclipsefp.haskell.scion.commands;

import org.json.JSONException;

public class DefinedNamesCommand extends ScionCommand {

	@Override
	protected String getMethod() {
		return "defined-names";
	}

	@Override
	protected void processResult(Object result) throws JSONException {
		// TODO
	}

}
