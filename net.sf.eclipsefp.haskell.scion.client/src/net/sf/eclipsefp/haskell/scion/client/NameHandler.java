package net.sf.eclipsefp.haskell.scion.client;

import java.util.List;

/**
 * Simple handler for a scion command that returns a list of names
 * @author JP Moresmau
 *
 */
public interface NameHandler {

	void nameResult(List<String> names);
}
