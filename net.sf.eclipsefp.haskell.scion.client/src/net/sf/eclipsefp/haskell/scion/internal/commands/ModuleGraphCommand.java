package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;

public class ModuleGraphCommand extends DefinedNamesCommand {

	public ModuleGraphCommand(IScionCommandRunner runner, IScionServer server) {
		super(runner, server);
	}

	@Override
	protected String getMethod() {
		return "module-graph";
	}
}
