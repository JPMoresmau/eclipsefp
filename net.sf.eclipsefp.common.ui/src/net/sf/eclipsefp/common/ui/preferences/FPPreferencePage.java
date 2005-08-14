// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences;

import net.sf.eclipsefp.common.ui.CommonUIPlugin;



/** <p>The preference page category for the top level node in the prefs menu,
  * all preference pages for fp tools have this as their parent category.</p>  
  * 
  * @author Leif Frenzel
  */
public class FPPreferencePage extends RootPreferencePage {

  public FPPreferencePage() {
    super(   "Preferences for Functional Programming.\n"
           + "Select one of the fp modules from the tree." );
    setPreferenceStore( CommonUIPlugin.getDefault().getPreferenceStore() );
  }
}