package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * 
 * @author JP Moresmau
 *
 */
public class ParseCabalCommand extends ScionCommand {
	private String fileName;
	private JSONObject description;
	
	public ParseCabalCommand(IScionCommandRunner runner, IScionServer server, String fileName) {
		super(runner, server, Job.BUILD);
		this.fileName=fileName;
	}
	
	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("cabal-file", fileName);
		return params;
	}
	
	@Override
	protected void doProcessResult(Object result) throws JSONException {
		description=(JSONObject)result; 
	}
	
	public JSONObject getDescription() {
		return description;
	}

	@Override
	protected String getMethod() {
		return "parse-cabal";
	}

}
