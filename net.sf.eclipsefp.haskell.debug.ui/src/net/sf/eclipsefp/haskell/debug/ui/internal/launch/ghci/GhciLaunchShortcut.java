// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci;

import net.sf.eclipsefp.haskell.debug.ui.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.debug.ui.internal.launch.InteractiveLaunchShortcut;


/** <p>launch shortcut for running GHCi.</p>
  *
  * @author Leif Frenzel
  */
public class GhciLaunchShortcut extends InteractiveLaunchShortcut {

  private IInteractiveLaunchOperationDelegate delegate;


  // interface methods of InteractiveLaunchOperation
  //////////////////////////////////////////////////

  @Override
  public IInteractiveLaunchOperationDelegate getDelegate() {
    if( delegate == null ) {
      delegate = new GhciLaunchOperationDelegate();
    }
    return delegate;
  }
}
