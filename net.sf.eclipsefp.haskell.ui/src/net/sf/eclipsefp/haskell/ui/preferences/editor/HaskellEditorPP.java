// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.preferences.editor;

import java.util.Iterator;

import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.editors.text.TextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.AnnotationPreference;
import org.eclipse.ui.texteditor.MarkerAnnotationPreferences;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;


/** <p>the preference page for the Haskell editor.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellEditorPP extends PreferencePage 
                             implements IWorkbenchPreferencePage, 
                                        IEditorPreferenceNames {

  private OverlayPreferenceStore overlayStore;

  
  public static void initializeDefaultValues( final IPreferenceStore store ) {
    TextEditorPreferenceConstants.initializeDefaultValues( store );
    DefaultEditorPreferenceInitializer.initializeDefaultValues( store );
  }
  

  // interface methods of PreferencePage
  //////////////////////////////////////
  
  protected Control createContents( final Composite parent ) {
    TabFolder folder = new TabFolder( parent, SWT.NONE );

    Tab appearanceTab = new AppearanceTab( overlayStore );
    createTab( folder, "Appeara&nce", appearanceTab.createControl( folder ) );
    
    Tab syntaxTab = new SyntaxTab( overlayStore );
    createTab( folder, "Synta&x", syntaxTab.createControl( folder ) );

    // TODO use this when reasonable completions can be made
//    Tab caTab = new ContentAssistTab( overlayStore );
//    createTab( folder, "&Content Assist", caTab.createControl( folder ) );
    
    Tab annotationsTab = new AnnotationsTab( overlayStore );
    createTab( folder, "Annotation&s", annotationsTab.createControl( folder ) );

    Tab typingTab = new TypingTab( overlayStore );
    createTab( folder, "T&yping", typingTab.createControl( folder ) );
    
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
    setDescription( "Haskell Editor settings:" );
    setPreferenceStore( HaskellUIPlugin.getDefault().getPreferenceStore() );

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
    MarkerAnnotationPreferences preferences = new MarkerAnnotationPreferences();
    IPreferenceStore prefStore = getPreferenceStore();
    OverlayPreferenceStore store = new OverlayPreferenceStore( prefStore );

    addAnnotationPreferences( preferences, store );
    addAppearancePreferences( store );
    addSyntaxPreferences( store );
    addCAPreferences( store );
    addTypingPreferences( store );

    return store;
  }

  private void addTypingPreferences( final OverlayPreferenceStore store ) {
    store.addBooleanKey( EDITOR_SPACES_FOR_TABS );
    store.addBooleanKey( EDITOR_CLOSE_STRINGS );
    store.addBooleanKey( EDITOR_CLOSE_BRACKETS_AND_PARENS );
    store.addBooleanKey( EDITOR_CLOSE_BRACES );
  }

  private void addCAPreferences( final OverlayPreferenceStore store ) {
    store.addBooleanKey( CA_AUTOINSERT );
    store.addBooleanKey( CA_ORDER_PROPOSALS );
    store.addBooleanKey( CA_AUTOACTIVATION );
    store.addIntKey( CA_AUTOACTIVATION_DELAY );
    store.addStringKey( CA_AUTOACTIVATION_TRIGGERS );
    store.addStringKey( CA_PROPOSALS_BACKGROUND );
    store.addStringKey( CA_PROPOSALS_FOREGROUND );
  }

  private void addSyntaxPreferences( final OverlayPreferenceStore store ) {
    store.addStringKey( EDITOR_FOREGROUND_COLOR );
    store.addBooleanKey( EDITOR_FOREGROUND_DEFAULT_COLOR );
    store.addStringKey( EDITOR_BACKGROUND_COLOR );
    store.addBooleanKey( EDITOR_BACKGROUND_DEFAULT_COLOR );
    store.addStringKey( EDITOR_COMMENT_COLOR );
    store.addBooleanKey( EDITOR_COMMENT_BOLD );
    store.addStringKey( EDITOR_LITERATE_COMMENT_COLOR );
    store.addBooleanKey( EDITOR_LITERATE_COMMENT_BOLD );
    store.addStringKey( EDITOR_STRING_COLOR );
    store.addBooleanKey( EDITOR_STRING_BOLD );
    store.addStringKey( EDITOR_FUNCTION_COLOR );
    store.addBooleanKey( EDITOR_FUNCTION_BOLD );
    store.addStringKey( EDITOR_KEYWORD_COLOR );
    store.addBooleanKey( EDITOR_KEYWORD_BOLD );
    store.addStringKey( EDITOR_DEFAULT_COLOR );
    store.addBooleanKey( EDITOR_DEFAULT_BOLD );
  }

  private void addAppearancePreferences( final OverlayPreferenceStore store ) {
    store.addIntKey( EDITOR_TAB_WIDTH );
    store.addBooleanKey( EDITOR_CURRENT_LINE );
    store.addStringKey( EDITOR_CURRENT_LINE_COLOR );
    store.addBooleanKey( EDITOR_MATCHING_BRACKETS );
    store.addStringKey( EDITOR_MATCHING_BRACKETS_COLOR );
    store.addBooleanKey( EDITOR_PRINT_MARGIN );
    store.addStringKey( EDITOR_PRINT_MARGIN_COLOR );
    store.addIntKey( EDITOR_PRINT_MARGIN_COLUMN );
    store.addBooleanKey( EDITOR_OVERVIEW_RULER );
    store.addStringKey( EDITOR_LINE_NUMBER_RULER_COLOR );
    store.addBooleanKey( EDITOR_LINE_NUMBER_RULER );
  }

  private void addAnnotationPreferences( 
        final MarkerAnnotationPreferences preferences, 
        final OverlayPreferenceStore store ) {
    Iterator iter = preferences.getAnnotationPreferences().iterator();
    while( iter.hasNext() ) {
      AnnotationPreference info = ( AnnotationPreference )iter.next();
      store.addStringKey( info.getColorPreferenceKey() );
      store.addBooleanKey( info.getTextPreferenceKey() );
      store.addBooleanKey( info.getOverviewRulerPreferenceKey() );
    }
  }
}