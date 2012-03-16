// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.ui.preferences;

import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.haskell.hugs.HugsPlugin;
import net.sf.eclipsefp.haskell.hugs.core.IHugsParameters;
import net.sf.eclipsefp.haskell.hugs.core.preferences.IHugsPreferenceNames;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.osgi.service.prefs.BackingStoreException;


/** <p>The preference page for the HUGS preferences.</p>
  *
  * @author Leif Frenzel
  */
public class HugsPreferencePage extends PreferencePage
                                implements IWorkbenchPreferencePage,
                                           IHugsPreferenceNames,
                                           IHugsParameters {

  private OverlayPreferenceStore overlayStore;


  // interface methods of PreferencePage
  //////////////////////////////////////

  @Override
  protected Control createContents( final Composite parent ) {
    TabFolder folder = new TabFolder( parent, SWT.NONE );

    Tab generalTab = new GeneralTab( overlayStore );
    createTab( folder, "General", generalTab.createControl( folder ) );

//    Tab languageTab = new LanguageTab( overlayStore );
//    Control languageControl = languageTab.createControl( folder );
//    createTab( folder, "Language Options", languageControl );
//
//    Tab optimizationTab = new OptimizationTab( overlayStore );
//    Control optimizationControl = optimizationTab.createControl( folder );
//    createTab( folder, "Optimization", optimizationControl );
//
//    Tab moreOptimizationTab = new MoreOptimizationTab( overlayStore );
//    Control moreOptControl = moreOptimizationTab.createControl( folder );
//    createTab( folder, "More Optimization", moreOptControl );

    Dialog.applyDialogFont( folder );
    return folder;
  }

  @Override
  public void dispose() {
    if( overlayStore != null ) {
      overlayStore.stopListening();
      overlayStore = null;
    }
    super.dispose();
  }

  @Override
  public boolean performOk() {
    overlayStore.propagate();
    try {
      new InstanceScope().getNode(HugsPlugin.getPluginId()).flush();
    } catch( BackingStoreException ex ) {
      HugsPlugin.log( ex );
    }
    return true;
  }

  @Override
  protected void performDefaults() {
    overlayStore.loadDefaults();
    super.performDefaults();
  }


  // interface methods of IWorkbenchPreferencePage
  ////////////////////////////////////////////////

  @Override
  public void init( final IWorkbench workbench ) {
    setPreferenceStore( HugsPlugin.getDefault().getPreferenceStore() );

    overlayStore = createOverlayStore();
    overlayStore.load();
    overlayStore.startListening();
  }


  // helping methods
  //////////////////

  private void createTab( final TabFolder folder,
                          final String label,
                          final Control control ) {
    TabItem tab = new TabItem( folder, SWT.NONE );
    tab.setText( label );
    tab.setControl( control );
  }

  private OverlayPreferenceStore createOverlayStore() {
    IPreferenceStore prefStore = getPreferenceStore();
    OverlayPreferenceStore store = new OverlayPreferenceStore( prefStore );

    addGeneralPreferences( store );

    return store;
  }

  private void addGeneralPreferences( final OverlayPreferenceStore store ) {
    store.addStringKey( EXECUTABLE_NAME );
  }
}