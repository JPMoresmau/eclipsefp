package net.sf.eclipsefp.haskell.scion.commands;

import net.sf.eclipsefp.haskell.scion.types.CompilationResult;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Command that loads a given component into the Scion server. 
 * 
 * @author Thomas ten Cate
 */
public class LoadCommand extends ScionCommand {

	private String fileName;
	private CompilationResult compilationResult;

	public LoadCommand(String fileName) {
		this.fileName = fileName;
	}
	
	@Override
	protected String getMethod() {
		return "load";
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		JSONObject component = new JSONObject();
		component.put("file", fileName);
		params.put("component", component);
		return params;
	}

	@Override
	protected void processResult(Object result) throws JSONException {
		compilationResult = new CompilationResult((JSONObject)result);
	}
	
	public CompilationResult getCompilationResult() {
		return compilationResult;
	}

}
