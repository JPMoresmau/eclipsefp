/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;


/**
 * Configure path to stylish-haskell
 * @author JP Moresmau
 *
 */
public class StylishHaskellPP extends ExecutablePP {

  public StylishHaskellPP(){
    super("stylish-haskell","stylish-haskell",IPreferenceConstants.STYLISHHASKELL_EXECUTABLE);
  }
}

