// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.editors.text.TextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.AnnotationPreference;
import org.eclipse.ui.texteditor.MarkerAnnotationPreferences;
import org.osgi.service.prefs.BackingStoreException;


/** <p>the preference page for the Haskell editor.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellEditorPP extends PreferencePage
                             implements IWorkbenchPreferencePage,
                                        IEditorPreferenceNames {

  private OverlayPreferenceStore overlayStore;
  private final List<Tab> tabs=new LinkedList<Tab>();

  public static void initializeDefaultValues( final IPreferenceStore store ) {
    TextEditorPreferenceConstants.initializeDefaultValues( store );
    DefaultEditorPreferenceInitializer.initializeDefaultValues( store );
  }


  // interface methods of PreferencePage
  //////////////////////////////////////

  @Override
  protected Control createContents( final Composite parent ) {
    TabFolder folder = new TabFolder( parent, SWT.NONE );

    Tab appearanceTab = new AppearanceTab( overlayStore );
    createTab( folder, UITexts.preferences_editor_appearance_title, appearanceTab );

    Tab syntaxTab = new SyntaxTab( overlayStore );
    createTab( folder, UITexts.preferences_editor_syntax_title, syntaxTab );

    Tab annotationsTab = new AnnotationsTab( overlayStore );
    createTab( folder, UITexts.preferences_editor_annotations_title, annotationsTab );

    Tab typingTab = new TypingTab( overlayStore );
    createTab( folder, UITexts.preferences_editor_typing_title, typingTab);

    return folder;
  }

  @Override
  public void dispose() {
    if( overlayStore != null ) {
      for (Tab t:tabs){
        t.dispose();
      }
      tabs.clear();
      overlayStore.stopListening();
      overlayStore = null;
    }
    super.dispose();
  }

  @Override
  public boolean performOk() {
    overlayStore.propagate();
    try {
      new InstanceScope().getNode(HaskellUIPlugin.getPluginId()).flush();
    } catch( BackingStoreException ex ) {
      HaskellUIPlugin.log( ex );
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

  public void init( final IWorkbench workbench ) {
    setDescription( UITexts.preferences_editor_description );
    setPreferenceStore( HaskellUIPlugin.getDefault().getPreferenceStore() );

    overlayStore = createOverlayStore();
    overlayStore.load();
    overlayStore.startListening();
  }


  // helping methods
  //////////////////

  private void createTab( final TabFolder folder,
                          final String label,
                          final Tab tab ) {
    TabItem tabItem = new TabItem( folder, SWT.NONE );
    tabItem.setText( label );
    tabItem.setControl( tab.createControl( folder ) );
    tabs.add(tab);
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
    store.addStringKey( EDITOR_CHAR_COLOR );
    store.addBooleanKey( EDITOR_CHAR_BOLD );
    store.addStringKey( EDITOR_FUNCTION_COLOR );
    store.addBooleanKey( EDITOR_FUNCTION_BOLD );
    store.addStringKey( EDITOR_KEYWORD_COLOR );
    store.addBooleanKey( EDITOR_KEYWORD_BOLD );
    store.addStringKey( EDITOR_DEFAULT_COLOR );
    store.addBooleanKey( EDITOR_DEFAULT_BOLD );
  }

  private void addAppearancePreferences( final OverlayPreferenceStore store ) {
    store.addIntKey( EDITOR_TAB_WIDTH );
    store.addIntKey( EDITOR_CABAL_TAB_WIDTH );
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