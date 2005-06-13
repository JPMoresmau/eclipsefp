// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.preferences;

import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;


/** <p>the preference page for the Haskell compilers.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellCompilerPP extends PreferencePage 
                               implements IWorkbenchPreferencePage,
                                          IPreferenceConstants {

  private OverlayPreferenceStore overlayStore;

  
  public static void initializeDefaultValues( final IPreferenceStore store ) {
    store.setDefault( SHOW_COMPILER_LOG, true );    
  }
  

  // interface methods of PreferencePage
  //////////////////////////////////////
  
  protected Control createContents( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    
    Tab tab = new HaskellCompilerTab( overlayStore );
    tab.createControl( parent );
    return composite;
  }

  public void dispose() {
    if( overlayStore != null ) {
      overlayStore.stopListening();
      overlayStore = null;
    }
    super.dispose();
  }

  public boolean performOk() {
    overlayStore.propagate();
    HaskellUIPlugin.getDefault().savePluginPreferences();
    return true;
  }

  protected void performDefaults() {
    overlayStore.loadDefaults();
    super.performDefaults();
  }

  
  // interface methods of IWorkbenchPreferencePage
  ////////////////////////////////////////////////
  
  public void init( final IWorkbench workbench ) {
    IPreferenceStore store = HaskellUIPlugin.getDefault().getPreferenceStore();
    setPreferenceStore( store );

    overlayStore = createOverlayStore();
    overlayStore.load();
    overlayStore.startListening();
  }


  // helping methods
  //////////////////

  private OverlayPreferenceStore createOverlayStore() {
    IPreferenceStore prefStore = getPreferenceStore();
    OverlayPreferenceStore store = new OverlayPreferenceStore( prefStore );

    store.addBooleanKey( SHOW_COMPILER_LOG );

    return store;
  }
}