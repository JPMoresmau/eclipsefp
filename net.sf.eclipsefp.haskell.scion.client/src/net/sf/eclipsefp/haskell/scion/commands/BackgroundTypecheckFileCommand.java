package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.types.CompilationResult;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class BackgroundTypecheckFileCommand extends ScionCommand {

	private String fileName;
	private CompilationResult compilationResult;
	
	public BackgroundTypecheckFileCommand(String fileName) {
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
	protected void processResult(Object json) throws JSONException {
		JSONArray result = (JSONArray)json;
		compilationResult = new CompilationResult(result.getJSONObject(1));
	}
	
	public CompilationResult getCompilationResult() {
		return compilationResult;
	}

}
