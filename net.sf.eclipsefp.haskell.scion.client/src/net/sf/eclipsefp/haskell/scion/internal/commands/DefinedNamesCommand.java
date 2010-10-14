package net.sf.eclipsefp.haskell.scion.internal.commands;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;

public class DefinedNamesCommand extends ScionCommand {
	private List<String> names=new ArrayList<String>();
	
	public DefinedNamesCommand(IScionCommandRunner runner, IScionServer server) {
		super(runner, server, Job.INTERACTIVE);
	}

	@Override
	protected String getMethod() {
		return "defined-names";
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		JSONArray arr = (JSONArray)result;
		for (int a=0;a<arr.length();a++){
			names.add(arr.getString(a));
		}
	}
	
	public List<String> getNames() {
		return names;
	}
}
