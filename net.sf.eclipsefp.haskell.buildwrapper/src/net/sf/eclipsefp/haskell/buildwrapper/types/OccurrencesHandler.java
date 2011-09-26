package net.sf.eclipsefp.haskell.buildwrapper.types;

import java.util.List;


/**
 * Async handler for occurrences
 * @author JP Moresmau
 *
 */
public interface OccurrencesHandler  {

	
	void handleOccurrences(List<Occurrence> occurrences);

}
