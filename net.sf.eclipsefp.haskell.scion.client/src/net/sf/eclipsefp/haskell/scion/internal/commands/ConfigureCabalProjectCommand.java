package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.json.JSONException;

public class ConfigureCabalProjectCommand extends OpenCabalProjectCommand {
	public ConfigureCabalProjectCommand(final IProject project) {
		super(project);
	}

	@Override
	protected void doProcessResult() throws JSONException {
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
  public String getMethod() {
		return "configure-cabal-project";
	}
	
}
