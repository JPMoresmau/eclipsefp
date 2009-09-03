package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class BackgroundTypecheckFileCommand extends ScionCommand {

	private String fileName;
	private CompilationResult compilationResult;
	
	public BackgroundTypecheckFileCommand(IScionCommandRunner runner, String fileName) {
		super(runner, Job.BUILD);
		this.fileName = fileName;
	}

	@Override
	protected String getMethod() {
		return "background-typecheck-file";
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("file", fileName);
		return params;
	}

	@Override
	protected void doProcessResult(Object json) throws JSONException {
		if (json instanceof JSONArray){
			JSONArray result = (JSONArray)json;
			compilationResult = new CompilationResult(result.getJSONObject(1));
		} else if (json instanceof JSONObject){
			compilationResult = new CompilationResult(((JSONObject)json).getJSONObject("Right"));
		}
	}
	
	public CompilationResult getCompilationResult() {
		return compilationResult;
	}

}
