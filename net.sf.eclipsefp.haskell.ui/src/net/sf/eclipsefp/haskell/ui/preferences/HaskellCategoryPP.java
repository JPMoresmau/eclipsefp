// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.preferences;

import net.sf.eclipsefp.common.ui.preferences.RootPreferencePage;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;


/** <p>the root preference page for the Haskell tools.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellCategoryPP extends RootPreferencePage {

  public HaskellCategoryPP() {
    super( UITexts.preferences_title );
    setPreferenceStore( HaskellUIPlugin.getDefault().getPreferenceStore() );
  }
}