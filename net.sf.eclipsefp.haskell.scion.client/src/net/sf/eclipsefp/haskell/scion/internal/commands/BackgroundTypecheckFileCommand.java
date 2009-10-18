package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;
import net.sf.eclipsefp.haskell.scion.types.ICompilerResult;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.Note;
import net.sf.eclipsefp.haskell.scion.types.Note.Kind;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class BackgroundTypecheckFileCommand extends ScionCommand implements ICompilerResult{

	private IFile file;
	private CompilationResult compilationResult;
	
	private ScionInstance instance;
	
	public BackgroundTypecheckFileCommand(ScionInstance runner, IFile file) {
		super(runner, Job.BUILD);
		this.file = file;
		this.instance=runner;
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
				instance.getLoadedFiles().add(file);
				compilationResult = new CompilationResult(cr);
			} else {
				String err=o.optString("Left");
				try {
					new Note(Kind.ERROR,new Location(file.getLocation().toOSString(),1,1,1,1),err,null).applyAsMarker(file);
				} catch (CoreException ce){
					ce.printStackTrace();
				}
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
