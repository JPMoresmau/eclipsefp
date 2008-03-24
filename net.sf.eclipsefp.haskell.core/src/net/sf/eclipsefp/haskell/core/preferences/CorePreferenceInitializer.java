// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.preferences;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;


/** <p>initializer for the core preferences (declared in the
  * <code>plugin.xml</code>).</p>
  *
  * @author Leif Frenzel
  */
public class CorePreferenceInitializer extends AbstractPreferenceInitializer
                                       implements ICorePreferenceNames {


  // interface methods of AbstractPreferenceInitializer
  /////////////////////////////////////////////////////

  @Override
  public void initializeDefaultPreferences() {
    Preferences preferences = getPreferences();
    preferences.setDefault( SELECTED_COMPILER, "ghcCompiler" ); //$NON-NLS-1$
    preferences.setDefault( FOLDERS_SRC, "src" ); //$NON-NLS-1$
    preferences.setDefault( FOLDERS_OUT, "out" ); //$NON-NLS-1$
    preferences.setDefault( TARGET_BINARY, "bin/theResult" ); //$NON-NLS-1$
    preferences.setDefault( FOLDERS_IN_NEW_PROJECT, true );
  }


  // helping methods
  //////////////////

  private Preferences getPreferences() {
    return HaskellCorePlugin.getDefault().getPluginPreferences();
  }
}