/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;


/**
 * @author JP Moresmau
 *
 */
public class CabalDevPP extends ExecutablePP {
  public CabalDevPP(){
    super("cabal-dev","cabal-dev",IPreferenceConstants.CABALDEV_EXECUTABLE);
  }
}
