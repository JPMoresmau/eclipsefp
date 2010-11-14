package net.sf.eclipsefp.haskell.scion.types;

import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;

public interface IAsyncScionCommandAction {
  /**
   * Handle the result or create additional actions related to asynchronously executed
   * Scion commands
   * 
   * @param command The command
   */
  public void handle( final ScionCommand command );
}
