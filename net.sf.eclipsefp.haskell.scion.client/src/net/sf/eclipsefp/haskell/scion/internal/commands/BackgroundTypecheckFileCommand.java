package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;
import net.sf.eclipsefp.haskell.scion.types.ICompilerResult;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class BackgroundTypecheckFileCommand extends ScionCommand implements ICompilerResult{

	private IFile file;
	private CompilationResult compilationResult;
	
	public BackgroundTypecheckFileCommand(IScionCommandRunner runner, IFile file) {
		super(runner, Job.BUILD);
		this.file = file;
	}

	@Override
	protected String getMethod() {
		return "background-typecheck-file";
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("file", file.getLocation().toOSString());
		return params;
	}

	@Override
	protected void doProcessResult(Object json) throws JSONException {
		if (json instanceof JSONArray){
			JSONArray result = (JSONArray)json;
			compilationResult = new CompilationResult(result.getJSONObject(1));
		} else if (json instanceof JSONObject){
			JSONObject o=(JSONObject)json;
			JSONObject cr=o.optJSONObject("Right");
			if (cr!=null){
				compilationResult = new CompilationResult(cr);
			}
		}
	}
	
	public CompilationResult getCompilationResult() {
		return compilationResult;
	}

	public boolean hasOutput() {
		return false;
	}
}
