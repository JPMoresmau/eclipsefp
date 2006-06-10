// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.util;

import net.sf.eclipsefp.haskell.cabal.ui.internal.CabalUIPlugin;

/** <p>contains constant names for images in the Cabal UI.</p>
  * 
  * @author Leif Frenzel
  */
public interface IImageNames {
  
  // prefix all constants with the plugin id
  String ID = CabalUIPlugin.getPluginId();
  
  String EXECUTABLE_STANZA = ID + ".EXECUTABLE_STANZA";
  String LIBRARY_STANZA    = ID + ".LIBRARY_STANZA";
  String GENERAL_STANZA    = ID + ".GENERAL_STANZA";
}