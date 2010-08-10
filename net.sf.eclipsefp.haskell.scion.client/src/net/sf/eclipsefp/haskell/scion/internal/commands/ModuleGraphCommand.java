package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

public class ModuleGraphCommand extends DefinedNamesCommand {

	public ModuleGraphCommand(IScionCommandRunner runner) {
		super(runner);
	}

	@Override
	protected String getMethod() {
		return "module-graph";
	}
}
