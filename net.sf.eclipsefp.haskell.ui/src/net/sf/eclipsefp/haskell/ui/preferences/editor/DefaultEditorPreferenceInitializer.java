// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.preferences.editor;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;

import net.sf.eclipsefp.haskell.ui.editor.text.ColorProvider;


/** <p>runs initialization code for default preference settings for the
  * Haskell editor.</p>
  * 
  * @author Leif Frenzel
  */
class DefaultEditorPreferenceInitializer implements IEditorPreferenceNames {

  static void initializeDefaultValues( final IPreferenceStore store ) {
    initAppearance( store );
    initColors( store );
    initCA( store );
    initTyping( store );
  }

  private static void initColors( final IPreferenceStore store ) {
    store.setDefault( EDITOR_FOREGROUND_DEFAULT_COLOR, true );
    store.setDefault( EDITOR_BACKGROUND_DEFAULT_COLOR, true );
    
    conv( store, EDITOR_COMMENT_COLOR, ColorProvider.DEFAULT_COMMENT );
    store.setDefault( EDITOR_COMMENT_BOLD, false );
    conv( store, 
          EDITOR_LITERATE_COMMENT_COLOR, 
          ColorProvider.DEFAULT_LITERATE_COMMENT );
    store.setDefault( EDITOR_LITERATE_COMMENT_BOLD, false );
    
    conv( store, EDITOR_FUNCTION_COLOR, ColorProvider.DEFAULT_FUNCTION );
    store.setDefault( EDITOR_FUNCTION_BOLD, true );
    conv( store, EDITOR_KEYWORD_COLOR, ColorProvider.DEFAULT_KEYWORD );
    store.setDefault( EDITOR_KEYWORD_BOLD, true );
    conv( store, EDITOR_STRING_COLOR, ColorProvider.DEFAULT_STRING );
    store.setDefault( EDITOR_STRING_BOLD, false );
    conv( store, EDITOR_DEFAULT_COLOR, ColorProvider.DEFAULT_OTHER );
    store.setDefault( EDITOR_DEFAULT_BOLD, false );
  }

  private static void initCA( final IPreferenceStore store ) {
    store.setDefault( CA_AUTOINSERT, false );
    store.setDefault( CA_ORDER_PROPOSALS, false );
    store.setDefault( CA_AUTOACTIVATION, true );
    store.setDefault( CA_AUTOACTIVATION_DELAY, 500 );
    store.setDefault( CA_AUTOACTIVATION_TRIGGERS, "." );
    conv( store, CA_PROPOSALS_BACKGROUND, new RGB( 254, 241, 233 ) );
    conv( store, CA_PROPOSALS_FOREGROUND, new RGB( 0, 0, 0 ) );
  }

  private static void initTyping( final IPreferenceStore store ) {
    store.setDefault( EDITOR_TAB_WIDTH, 4 );
    store.setDefault( EDITOR_SPACES_FOR_TABS, false );
    store.setDefault( EDITOR_CLOSE_STRINGS, true );
    store.setDefault( EDITOR_CLOSE_BRACKETS_AND_PARENS, true );
    store.setDefault( EDITOR_CLOSE_BRACES, true );
  }

  private static void initAppearance( final IPreferenceStore store ) {
    store.setDefault( EDITOR_TAB_WIDTH, 8 );
    store.setDefault( EDITOR_PRINT_MARGIN_COLUMN, 80 );

    store.setDefault( EDITOR_OVERVIEW_RULER, true );
    store.setDefault( EDITOR_LINE_NUMBER_RULER , false );
    store.setDefault( EDITOR_MATCHING_BRACKETS, true );
    store.setDefault( EDITOR_CURRENT_LINE, true );
    store.setDefault( EDITOR_PRINT_MARGIN, true );
    
    conv( store, EDITOR_MATCHING_BRACKETS_COLOR, new RGB( 192, 192, 192 ) );
  }
  
  private static void conv( final IPreferenceStore store, 
                            final String key, 
                            final RGB rgb ) {
    PreferenceConverter.setDefault( store, key, rgb );
  }
}