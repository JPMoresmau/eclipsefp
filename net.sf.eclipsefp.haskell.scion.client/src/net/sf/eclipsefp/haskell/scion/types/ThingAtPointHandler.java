package net.sf.eclipsefp.haskell.scion.types;

import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;

/**
 * Async handler for thing at point
 * @author JP Moresmau
 *
 */
public abstract class ThingAtPointHandler implements IAsyncScionCommandAction {

	public void handle(ScionCommand command) {
		handleThing(((ThingAtPointCommand)command).getThing());
	}

	public abstract void handleThing(String thing);
	
}
