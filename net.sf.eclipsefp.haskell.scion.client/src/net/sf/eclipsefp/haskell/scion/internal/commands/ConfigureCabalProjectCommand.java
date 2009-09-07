package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

import org.eclipse.core.resources.IProject;

public class ConfigureCabalProjectCommand extends OpenCabalProjectCommand {

	public ConfigureCabalProjectCommand(IScionCommandRunner runner,
			int priority, IProject p) {
		super(runner, priority, p);
	}

	@Override
	protected String getMethod() {
		return "configure-cabal-project";
	}
	
}
