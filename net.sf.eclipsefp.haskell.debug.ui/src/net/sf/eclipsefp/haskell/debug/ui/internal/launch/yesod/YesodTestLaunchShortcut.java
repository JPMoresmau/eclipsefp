/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.yesod;

import net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.YesodTestLaunchDelegate;


/**
 * Launch shortcut for yesod integration tests
 * @author JP Moresmau
 *
 */
public class YesodTestLaunchShortcut extends YesodDevelLaunchShortcut {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.yesod.YesodDevelLaunchShortcut#getDelegate()
   */
  @Override
  public IInteractiveLaunchOperationDelegate getDelegate() {
    if( delegate == null ) {
      delegate = new YesodTestLaunchOperationDelegate();
    }
    return delegate;
  }

  @Override
  protected String getConfigTypeName() {
    return YesodTestLaunchDelegate.class.getName();
  }
}
