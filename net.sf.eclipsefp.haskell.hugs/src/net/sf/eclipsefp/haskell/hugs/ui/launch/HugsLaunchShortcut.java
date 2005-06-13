//Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.ui.launch;

import de.leiffrenzel.fp.haskell.ui.launch.IInteractiveLaunchOperationDelegate;
import de.leiffrenzel.fp.haskell.ui.launch.InteractiveLaunchShortcut;

/** <p>launch shortcut for running GHCi.</p>
  * 
  * @author Leif Frenzel
  */
public class HugsLaunchShortcut extends InteractiveLaunchShortcut {

  
  private IInteractiveLaunchOperationDelegate delegate;

  
  // interface methods of InteractiveLaunchOperation
  //////////////////////////////////////////////////

  public IInteractiveLaunchOperationDelegate getDelegate() {
    if( delegate == null ) {
      delegate = new HugsLaunchOperationDelegate();
    }
    return delegate;
  }
}
