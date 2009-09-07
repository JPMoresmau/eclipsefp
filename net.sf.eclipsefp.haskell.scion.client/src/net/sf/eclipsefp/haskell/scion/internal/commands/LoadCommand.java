package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.types.CompilationResult;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.ICompilerResult;

import org.eclipse.core.runtime.jobs.Job;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Command that loads a given component into the Scion server. 
 * 
 * @author Thomas ten Cate
 */
public class LoadCommand extends ScionCommand implements ICompilerResult{

	
	private Component comp;
	private CompilationResult compilationResult;
	private boolean build;
	
	public LoadCommand(IScionCommandRunner runner, Component c,boolean build) {
		super(runner, Job.BUILD);
		this.comp=c;
		this.build=build;
	}
	
	@Override
	protected String getMethod() {
		return "load";
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("component", comp.toJSON());
		params.put("build",build);
		return params;
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		compilationResult = new CompilationResult((JSONObject)result);
	}
	
	public CompilationResult getCompilationResult() {
		return compilationResult;
	}

}
