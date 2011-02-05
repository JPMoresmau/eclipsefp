// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import org.eclipse.ui.texteditor.AbstractTextEditor;

/** <p>contains the preference names for the Haskell editor.</p>
  *
  * @author Leif Frenzel
  */
public interface IEditorPreferenceNames {

  // Inherited constants
//  String EDITOR_CURRENT_LINE
//   = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE;
//  String EDITOR_CURRENT_LINE_COLOR
//    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE_COLOR;
//  String EDITOR_LINE_NUMBER_RULER
//    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER;
//  String EDITOR_LINE_NUMBER_RULER_COLOR
//    = AbstractDecoratedTextEditorPreferenceConstants
//        .EDITOR_LINE_NUMBER_RULER_COLOR;
//  String EDITOR_PRINT_MARGIN
//    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN;
//  String EDITOR_PRINT_MARGIN_COLOR
//    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLOR;
//  String EDITOR_PRINT_MARGIN_COLUMN
//    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLUMN;
//  String EDITOR_OVERVIEW_RULER
//    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_OVERVIEW_RULER;
  String EDITOR_BACKGROUND_DEFAULT_COLOR
    = AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND_SYSTEM_DEFAULT;
  String EDITOR_BACKGROUND_COLOR
    = AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND;
  String EDITOR_FOREGROUND_COLOR
    = AbstractTextEditor.PREFERENCE_COLOR_FOREGROUND;
  String EDITOR_FOREGROUND_DEFAULT_COLOR
   = AbstractTextEditor.PREFERENCE_COLOR_FOREGROUND_SYSTEM_DEFAULT;

  // Custom constants
  String EDITOR_TAB_WIDTH                  = "tabWidth";  //$NON-NLS-1$
  String EDITOR_CABAL_TAB_WIDTH            = "tabWidthCabal";  //$NON-NLS-1$
  String EDITOR_MATCHING_BRACKETS          = "matchingBrackets";  //$NON-NLS-1$
  String EDITOR_MATCHING_BRACKETS_COLOR    = "matchingBracketsColor";  //$NON-NLS-1$
  String EDITOR_COMMENT_COLOR              = "commentColor";  //$NON-NLS-1$
  String EDITOR_COMMENT_BOLD               = "commentBold";  //$NON-NLS-1$
  String EDITOR_LITERATE_COMMENT_COLOR     = "literateCommentColor";  //$NON-NLS-1$
  String EDITOR_LITERATE_COMMENT_BOLD      = "literateCommentBold";  //$NON-NLS-1$
  String EDITOR_FUNCTION_COLOR             = "functionColor";  //$NON-NLS-1$
  String EDITOR_FUNCTION_BOLD              = "propertyBold";  //$NON-NLS-1$
  String EDITOR_KEYWORD_COLOR              = "keywordColor";  //$NON-NLS-1$
  String EDITOR_KEYWORD_BOLD               = "keywordBold";  //$NON-NLS-1$
  String EDITOR_STRING_COLOR               = "stringColor";  //$NON-NLS-1$
  String EDITOR_STRING_BOLD                = "stringBold";  //$NON-NLS-1$
  String EDITOR_CHAR_COLOR                 = "charColor";  //$NON-NLS-1$
  String EDITOR_CHAR_BOLD                  = "charBold";  //$NON-NLS-1$
  String EDITOR_NUMBER_COLOR               = "numberColor";  //$NON-NLS-1$
  String EDITOR_NUMBER_BOLD                = "numberBold";  //$NON-NLS-1$
  String EDITOR_VAR_COLOR                  = "varColor";  //$NON-NLS-1$
  String EDITOR_VAR_BOLD                   = "varBold";  //$NON-NLS-1$
  String EDITOR_CON_COLOR                  = "conColor";  //$NON-NLS-1$
  String EDITOR_CON_BOLD                   = "conBold";  //$NON-NLS-1$
  String EDITOR_SYMBOL_COLOR               = "symbolColor";  //$NON-NLS-1$
  String EDITOR_SYMBOL_BOLD                = "symbolBold";  //$NON-NLS-1$
  String EDITOR_CPP_COLOR                  = "cppColor";  //$NON-NLS-1$
  String EDITOR_CPP_BOLD                   = "cppBold";  //$NON-NLS-1$
  String EDITOR_TH_COLOR                   = "thColor";  //$NON-NLS-1$
  String EDITOR_TH_BOLD                    = "thBold";  //$NON-NLS-1$

  String EDITOR_DEFAULT_COLOR              = "defaultColor";  //$NON-NLS-1$
  String EDITOR_DEFAULT_BOLD               = "defaultBold";  //$NON-NLS-1$
  String EDITOR_SPACES_FOR_TABS            = "spacesForTabs";  //$NON-NLS-1$
  String EDITOR_CLOSE_STRINGS              = "closeStrings";  //$NON-NLS-1$
  String EDITOR_CLOSE_BRACKETS_AND_PARENS  = "closeBracketsAndParens";  //$NON-NLS-1$
  String EDITOR_CLOSE_BRACES               = "closeBraces";  //$NON-NLS-1$
  String EDITOR_SHOW_SELECTED_ELEMENT_ONLY = "showSelectedElementOnly";  //$NON-NLS-1$

  String CA_AUTOACTIVATION          = "contentAssistAutoActivation";  //$NON-NLS-1$
  String CA_AUTOACTIVATION_DELAY    = "contentAssistAutoActivationDelay";  //$NON-NLS-1$
  String CA_AUTOACTIVATION_TRIGGERS = "contentAssistAutoActivationTriggers";  //$NON-NLS-1$
  String CA_AUTOINSERT              = "contentAssistAutoInsert";  //$NON-NLS-1$
  String CA_ORDER_PROPOSALS         = "contentAssistOrderProposals";  //$NON-NLS-1$
  String CA_PROPOSALS_BACKGROUND    = "contentAssistProposalsBackground";  //$NON-NLS-1$
  String CA_PROPOSALS_FOREGROUND    = "contentAssistProposalsForeground";  //$NON-NLS-1$
}