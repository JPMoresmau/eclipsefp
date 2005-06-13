// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import de.leiffrenzel.fp.haskell.ghccompiler.GhcCompilerPlugin;
import de.leiffrenzel.fp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;

/** <p>preference page for settings for lauching interactive (GHCi) 
  * sessions.</p>
  * 
  * @author Leif Frenzel
  */
public class GhciPreferencePage extends PreferencePage 
                                implements IWorkbenchPreferencePage {

  private OverlayPreferenceStore overlayStore;

  // interface methods of PreferencePage
  //////////////////////////////////////
  
  protected Control createContents( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );
    
    Label lblExecutableInfo = new Label( composite, SWT.WRAP );
    String executableInfo =   "GHCi uses the same exectuable as the GHC " 
                            + "compiler.\nTo configure it, please use the " 
                            + "GHC preference page.";
    lblExecutableInfo.setText( executableInfo );
    
    Tab tab = new GhciTab( overlayStore );
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
    GhcCompilerPlugin.getDefault().savePluginPreferences();
    return true;
  }

  protected void performDefaults() {
    overlayStore.loadDefaults();
    super.performDefaults();
  }

  
  // interface methods of IWorkbenchPreferencePage
  ////////////////////////////////////////////////
  
  public void init( final IWorkbench workbench ) {
    IPreferenceStore ps = GhcCompilerPlugin.getDefault().getPreferenceStore();
    setPreferenceStore( ps );

    overlayStore = createOverlayStore();
    overlayStore.load();
    overlayStore.startListening();
  }


  // helping methods
  //////////////////

  private OverlayPreferenceStore createOverlayStore() {
    IPreferenceStore prefStore = getPreferenceStore();
    OverlayPreferenceStore store = new OverlayPreferenceStore( prefStore );

    store.addBooleanKey( IGhcPreferenceNames.GHCI_USES_GHC_OPTIONS );

    return store;
  }
}
