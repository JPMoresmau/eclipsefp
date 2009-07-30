//Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.core.preferences;

import net.sf.eclipsefp.haskell.hugs.HugsPlugin;
import org.eclipse.core.runtime.preferences.*;

/** <p>initializer for the HUGS preferences (declared in the
  * <code>plugin.xml</code>).</p>
  *
  * @author Leif Frenzel
  */
public class PreferenceInitializer extends AbstractPreferenceInitializer
                                   implements IHugsPreferenceNames {


  // interface methods of AbstractPreferenceInitializer
  /////////////////////////////////////////////////////

  @Override
  public void initializeDefaultPreferences() {
    IEclipsePreferences prefs = new DefaultScope().getNode( HugsPlugin.getPluginId() );
    initializeDefaultValues( prefs );
  }


  // helping methods
  //////////////////

  private void initializeDefaultValues( final IEclipsePreferences prefs ) {
    prefs.put( EXECUTABLE_NAME, "hugs" );
    // TODO more
  }
}
