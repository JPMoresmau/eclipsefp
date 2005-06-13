// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import de.leiffrenzel.fp.haskell.ghccompiler.GhcCompilerPlugin;
import de.leiffrenzel.fp.haskell.ghccompiler.core.IGhcParameters;
import de.leiffrenzel.fp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;


/** <p>The preference page for the GHC compiler preferences.</p>
  * 
  * @author Leif Frenzel
  */
public class GhcPreferencePage extends PreferencePage 
                               implements IWorkbenchPreferencePage,
                                          IGhcPreferenceNames,
                                          IGhcParameters {

  private OverlayPreferenceStore overlayStore;  

  
  // interface methods of PreferencePage
  //////////////////////////////////////
  
  protected Control createContents( final Composite parent ) {
    TabFolder folder = new TabFolder( parent, SWT.NONE );

    Tab generalTab = new GeneralTab( overlayStore );
    createTab( folder, "General", generalTab.createControl( folder ) );

    Tab languageTab = new LanguageTab( overlayStore );
    Control languageControl = languageTab.createControl( folder );
    createTab( folder, "Language Options", languageControl );

    Tab optimizationTab = new OptimizationTab( overlayStore );
    Control optimizationControl = optimizationTab.createControl( folder );
    createTab( folder, "Optimization", optimizationControl );

    Tab moreOptimizationTab = new MoreOptimizationTab( overlayStore );
    Control moreOptControl = moreOptimizationTab.createControl( folder );
    createTab( folder, "More Optimization", moreOptControl );
    
    Dialog.applyDialogFont( folder );
    return folder;
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
    setDescription( "GHC compiler settings" );
    setPreferenceStore( GhcCompilerPlugin.getDefault().getPreferenceStore() );

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
    addLanguagePrefs( store );
    addOptimizationPrefs( store );
    addMoreOptimizationPrefs( store );
    
    return store;
  }

  private void addGeneralPreferences( final OverlayPreferenceStore store ) {
    store.addStringKey( EXECUTABLE_NAME );
    store.addStringKey( EXTRA_OPTIONS );
    store.addBooleanKey( USE_EXTRA_OPTIONS );
  }

  private void addLanguagePrefs( final OverlayPreferenceStore store ) {
    // boolean preferences use the parameter as key
    store.addBooleanKey( LANG_GLASGOW_EXTS );
    store.addBooleanKey( LANG_FI );
    store.addBooleanKey( LANG_FFI );
    store.addBooleanKey( LANG_WITH );
    store.addBooleanKey( LANG_NO_MONOMORPHISM_RESTRICTION );
    store.addBooleanKey( LANG_ALLOW_OVERLAPPING_INSTANCES );
    store.addBooleanKey( LANG_ALLOW_UNDECIDABLE_INSTANCES );
    store.addBooleanKey( LANG_ALLOW_INCOHERENT_INSTANCES );
    store.addBooleanKey( LANG_GENERICS );
    store.addBooleanKey( LANG_NO_IMPLICIT_PRELUDE );
  }
  
  private void addOptimizationPrefs( final OverlayPreferenceStore store ) {
    store.addIntKey( OPTIMIZATION_LEVEL );
    // boolean preferences use the parameter as key
    store.addBooleanKey( OPT_EXCESS_PRECISION );
    store.addBooleanKey( OPT_IGNORE_ASSERTS );
    store.addBooleanKey( OPT_NO_STRICTNESS );
    store.addBooleanKey( OPT_NO_CPR );
    store.addBooleanKey( OPT_UNBOX_STRICT_FIELDS );    
  }

  private void addMoreOptimizationPrefs( final OverlayPreferenceStore store ) {
    // boolean preferences use the parameter as key
    store.addBooleanKey( OPT_CASE_MERGE );
    store.addBooleanKey( OPT_DICTS_STRICT );
    store.addBooleanKey( OPT_DO_ETA_REDUCTION );
    store.addBooleanKey( OPT_DO_LAMBDA_ETA_EXPANSION );
    store.addBooleanKey( OPT_FOLDR_BUILD_ON );
    store.addBooleanKey( OPT_IGNORE_INTERFACE_PRAGMAS );
    store.addBooleanKey( OPT_LET_NO_ESCAPE );
    store.addBooleanKey( OPT_OMIT_INTERFACE_PRAGMAS );
    store.addBooleanKey( OPT_NO_CSE );
    store.addBooleanKey( OPT_NO_PRE_INLINING );    
    store.addBooleanKey( OPT_NUMBERS_STRICT );
    store.addBooleanKey( OPT_USAGESP );
  }
}