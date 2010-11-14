package net.sf.eclipsefp.haskell.scion.types;

import java.util.List;

import net.sf.eclipsefp.haskell.scion.internal.commands.OutlineCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

/**
 * 
 * @author JP Moresmau
 *
 */
public abstract class OutlineHandler implements IAsyncScionCommandAction {

  public void handle ( final ScionCommand cmd ) {
    // Coerce and pass to handleOutline
    handleOutline( ((OutlineCommand) cmd).getOutlineDefs());
  }
  
  public abstract void handleOutline(List<OutlineDef> defs);
}
