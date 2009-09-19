package net.sf.eclipsefp.haskell.scion.client;

import java.util.List;

import net.sf.eclipsefp.haskell.scion.types.OutlineDef;

/**
 * Callback interface for outline command
 * @author JP Moresmau
 *
 */
public interface OutlineHandler {

	void outlineResult(List<OutlineDef> outlineDefs);
}
