package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.client.IScionServer;
import net.sf.eclipsefp.haskell.scion.internal.servers.IScionCommandRunner;

/**
 * 
 * @author JP Moresmau
 *
 */
public class ListExposedModulesCommand extends DefinedNamesCommand {

	public ListExposedModulesCommand(IScionCommandRunner runner, IScionServer server){
		super(runner, server);
	}
	
	@Override
	protected String getMethod() {
		return "list-exposed-modules";
	}

}
