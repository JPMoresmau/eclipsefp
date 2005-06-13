// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.preferences;

import net.sf.eclipsefp.common.ui.preferences.RootPreferencePage;
import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;


/** <p>the root preference page for the Haskell tools.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellCategoryPP extends RootPreferencePage {

  public HaskellCategoryPP() {
    super( "Preferences for Haskell.\nPlease select from the tree." );
    setPreferenceStore( HaskellUIPlugin.getDefault().getPreferenceStore() );
  }
}