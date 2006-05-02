//Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.ui.launch;

import net.sf.eclipsefp.haskell.ui.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.ui.launch.InteractiveLaunchShortcut;

/** <p>launch shortcut for running GHCi.</p>
  * 
  * @author Leif Frenzel
  */
public class GhciLaunchShortcut extends InteractiveLaunchShortcut {

  private IInteractiveLaunchOperationDelegate delegate;

  
  // interface methods of InteractiveLaunchOperation
  //////////////////////////////////////////////////

  public IInteractiveLaunchOperationDelegate getDelegate() {
    if( delegate == null ) {
      delegate = new GhciLaunchOperationDelegate();
    }
    return delegate;
  }
}
