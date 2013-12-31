/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;


/**
 *
 * @author JP Moresmau
 *
 */
public class CabalBenchLaunchShortcut extends CabalTestLaunchShortcut {

  /**
   *
   */
  public CabalBenchLaunchShortcut() {
  }

  @Override
  protected CabalBenchLaunchOperation getLaunchOperation(){
    return new CabalBenchLaunchOperation();
  }
}
