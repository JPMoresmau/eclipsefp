//Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.core.preferences;

import net.sf.eclipsefp.haskell.hugs.HugsPlugin;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;

/** <p>initializer for the HUGS preferences (declared in the 
  * <code>plugin.xml</code>).</p>
  *
  * @author Leif Frenzel
  */
public class PreferenceInitializer extends AbstractPreferenceInitializer
                                   implements IHugsPreferenceNames {

  
  // interface methods of AbstractPreferenceInitializer
  /////////////////////////////////////////////////////
  
  public void initializeDefaultPreferences() {
    Preferences prefs = HugsPlugin.getDefault().getPluginPreferences();
    initializeDefaultValues( prefs );
  }

  
  // helping methods
  //////////////////
  
  private void initializeDefaultValues( final Preferences prefs ) {
    prefs.setDefault( EXECUTABLE_NAME, "hugs" );
    // TODO more
  }
}
