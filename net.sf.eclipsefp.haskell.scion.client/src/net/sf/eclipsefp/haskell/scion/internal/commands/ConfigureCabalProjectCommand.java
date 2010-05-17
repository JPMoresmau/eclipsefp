package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.json.JSONException;

public class ConfigureCabalProjectCommand extends OpenCabalProjectCommand {

	public ConfigureCabalProjectCommand(IScionCommandRunner runner,
			int priority) {
		super(runner, priority);
	}

	@Override
	protected void doProcessResult(Object result) throws JSONException {
		try {
			IResource res=getProject().findMember(ScionPlugin.DIST_FOLDER);
			if (res!=null){
				res.refreshLocal(IResource.DEPTH_INFINITE, null);
			} else {
				getProject().refreshLocal(IResource.DEPTH_INFINITE, null);
			}
		} catch (CoreException ce){
			
		}
		
	}
	
	@Override
	protected String getMethod() {
		return "configure-cabal-project";
	}
	
}
