package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

import org.eclipse.core.resources.IProject;
import org.json.JSONException;
import org.json.JSONObject;

public class OpenCabalProjectCommand extends ScionCommand {
	private IProject project;
	
	public OpenCabalProjectCommand(IScionCommandRunner runner, int priority,IProject p) {
		super(runner, priority);
		this.project=p;
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		// NOOP
	}

	@Override
	protected String getMethod() {
		return "open-cabal-project";
	}
	
	public IProject getProject() {
		return project;
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("root-dir", getProject().getLocation().toOSString());
		return params;
	}
}
