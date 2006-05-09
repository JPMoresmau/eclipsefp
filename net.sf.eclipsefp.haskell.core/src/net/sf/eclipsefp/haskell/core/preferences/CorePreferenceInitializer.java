// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.preferences;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;


/** <p>initializer for the core preferences (declared in the 
  * <code>plugin.xml</code>).</p>
  * 
  * @author Leif Frenzel
  */
public class CorePreferenceInitializer extends AbstractPreferenceInitializer
                                       implements ICorePreferenceNames {

  
  // interface methods of AbstractPreferenceInitializer
  /////////////////////////////////////////////////////
  
  public void initializeDefaultPreferences() {
    Preferences preferences = getPreferences();    
//    store.setDefault( SELECTED_COMPILER, 
//                      DefaultHaskellCompiler.class.getName() );
    // TODO for the moment hardcoded ghc
    preferences.setDefault( SELECTED_COMPILER, "ghcCompiler" );
  }
  
  
  // helping methods
  //////////////////

  private Preferences getPreferences() {
    return HaskellCorePlugin.getDefault().getPluginPreferences();
  }
}