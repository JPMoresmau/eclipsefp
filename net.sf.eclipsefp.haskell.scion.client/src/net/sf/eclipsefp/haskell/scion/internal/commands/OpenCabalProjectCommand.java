package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;

import org.eclipse.core.resources.IProject;
import org.json.JSONException;
import org.json.JSONObject;

public class OpenCabalProjectCommand extends ScionCommand {
	
	public OpenCabalProjectCommand(IScionCommandRunner runner, IScionServer server, int priority) {
		super(runner, server, priority);
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
		return getRunner().getProject();
	}

	@Override
	protected JSONObject getParams() throws JSONException {
		JSONObject params = new JSONObject();
		params.put("root-dir", getProject().getLocation().toOSString());
		params.put("dist-dir",ScionPlugin.DIST_FOLDER);
		
		return params;
	}
}
