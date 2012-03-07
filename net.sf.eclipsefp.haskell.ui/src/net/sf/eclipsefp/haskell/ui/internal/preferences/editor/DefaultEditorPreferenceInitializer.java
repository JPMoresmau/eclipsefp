// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ColorProvider;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


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

    initTaskTags(store);

  }

  private static void initTaskTags( final IPreferenceStore store ) {
    store.setDefault( EDITOR_TASK_TAGS_CASE, false );
    try {
      JSONObject obj=new JSONObject();
      JSONArray high=new JSONArray();
      high.put( "FIXME" );
      obj.put( EDITOR_TASK_TAGS_HIGH, high );

      JSONArray normal=new JSONArray();
      normal.put( "TODO" );
      obj.put( EDITOR_TASK_TAGS_NORMAL, normal );

      JSONArray low=new JSONArray();
      low.put( "XXX" );
      obj.put( EDITOR_TASK_TAGS_LOW, low );
      store.setDefault( EDITOR_TASK_TAGS, obj.toString() );

    } catch (JSONException je){
      HaskellUIPlugin.log( je );
    }
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
    conv( store, EDITOR_CHAR_COLOR, ColorProvider.DEFAULT_CHAR );
    store.setDefault( EDITOR_CHAR_BOLD, false );
    conv( store, EDITOR_NUMBER_COLOR, ColorProvider.DEFAULT_NUMBER );
    store.setDefault( EDITOR_NUMBER_BOLD, false );
    conv( store, EDITOR_VAR_COLOR, ColorProvider.DEFAULT_VAR );
    store.setDefault( EDITOR_VAR_BOLD, false );
    conv( store, EDITOR_CON_COLOR, ColorProvider.DEFAULT_CON );
    store.setDefault( EDITOR_CON_BOLD, true );
    conv( store, EDITOR_SYMBOL_COLOR, ColorProvider.DEFAULT_SYMBOL );
    store.setDefault( EDITOR_SYMBOL_BOLD, false );
    conv( store, EDITOR_CPP_COLOR, ColorProvider.DEFAULT_CPP );
    store.setDefault( EDITOR_CPP_BOLD, true );
    conv( store, EDITOR_TH_COLOR, ColorProvider.DEFAULT_TH );
    store.setDefault( EDITOR_TH_BOLD, false );
    conv( store, EDITOR_DEFAULT_COLOR, ColorProvider.DEFAULT_OTHER );
    store.setDefault( EDITOR_DEFAULT_BOLD, false );
  }

  private static void initCA( final IPreferenceStore store ) {
    store.setDefault( CA_AUTOINSERT, false );
    store.setDefault( CA_ORDER_PROPOSALS, false );
    store.setDefault( CA_AUTOACTIVATION, true );
    store.setDefault( CA_AUTOACTIVATION_DELAY, 500 );
    store.setDefault( CA_AUTOACTIVATION_TRIGGERS, "" ); //$NON-NLS-1$
    conv( store, CA_PROPOSALS_BACKGROUND, new RGB( 254, 241, 233 ) );
    conv( store, CA_PROPOSALS_FOREGROUND, new RGB( 0, 0, 0 ) );
  }

  private static void initTyping( final IPreferenceStore store ) {
    // the tab width is specified by the Haskell report to be always 8
    store.setDefault( EDITOR_TAB_WIDTH, 8 );
    store.setDefault( EDITOR_CABAL_TAB_WIDTH, 2 );
    // good practice, no?
    //store.setDefault( EDITOR_SPACES_FOR_TABS, true );
    store.setDefault( EDITOR_CLOSE_STRINGS, true );
    store.setDefault( EDITOR_CLOSE_BRACKETS_AND_PARENS, true );
    store.setDefault( EDITOR_CLOSE_BRACES, true );
  }

  private static void initAppearance( final IPreferenceStore store ) {
    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLUMN );
    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLOR );
    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN );
//    store.setDefault( EDITOR_PRINT_MARGIN_COLUMN, 80 );

    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_OVERVIEW_RULER);
//    store.setDefault( EDITOR_OVERVIEW_RULER, true );
    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER );
    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER_COLOR );
//    store.setDefault( EDITOR_LINE_NUMBER_RULER , false );
    store.setDefault( EDITOR_MATCHING_BRACKETS, true );
    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE );
    store.setToDefault( AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN );
//    store.setDefault( EDITOR_CURRENT_LINE, true );
//    store.setDefault( EDITOR_PRINT_MARGIN, true );

    conv( store, EDITOR_MATCHING_BRACKETS_COLOR, new RGB( 192, 192, 192 ) );
  }

  private static void conv( final IPreferenceStore store,
                            final String key,
                            final RGB rgb ) {
    PreferenceConverter.setDefault( store, key, rgb );
  }
}