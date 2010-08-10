package net.sf.eclipsefp.haskell.scion.internal.commands;

import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;

/**
 * 
 * @author JP Moresmau
 *
 */
public class ListExposedModulesCommand extends DefinedNamesCommand {

	public ListExposedModulesCommand(IScionCommandRunner runner){
		super(runner);
	}
	
	@Override
	protected String getMethod() {
		return "list-exposed-modules";
	}

}
