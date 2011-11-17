/**
 *
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.IInteractiveLaunchOperationDelegate;


/**
 * @author Alejandro Serrano
 *
 */
public class GhciInteractiveLaunchShortcut extends InteractiveLaunchShortcut {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.young.InteractiveLaunchShortcut#getDelegate()
   */
  @Override
  public IInteractiveLaunchOperationDelegate getDelegate() {
    return new GhciLaunchOperationDelegate();
  }

}
