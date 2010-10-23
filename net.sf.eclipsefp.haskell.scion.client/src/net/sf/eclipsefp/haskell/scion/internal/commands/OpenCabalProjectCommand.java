package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

import org.eclipse.core.resources.IProject;
import org.json.JSONException;
import org.json.JSONObject;

public class OpenCabalProjectCommand extends ScionCommand {
	private final IProject project;
	
	public OpenCabalProjectCommand(IProject project) {
		super();
		this.project = project;
	}

	@Override
	protected void doProcessResult() throws JSONException {
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
		params.put("dist-dir",ScionPlugin.DIST_FOLDER);
		
		return params;
	}
}
