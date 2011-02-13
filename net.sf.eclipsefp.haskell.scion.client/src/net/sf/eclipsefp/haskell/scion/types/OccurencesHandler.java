package net.sf.eclipsefp.haskell.scion.types;

import net.sf.eclipsefp.haskell.scion.internal.commands.OccurrencesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

/**
 * Async handler for occurrences
 * @author JP Moresmau
 *
 */
public abstract class OccurencesHandler implements IAsyncScionCommandAction {

	public void handle(ScionCommand command) {
		handle(((OccurrencesCommand)command).getOccurrences());
	}
	
	public abstract void handle(Occurrence[] occurrences);

}
