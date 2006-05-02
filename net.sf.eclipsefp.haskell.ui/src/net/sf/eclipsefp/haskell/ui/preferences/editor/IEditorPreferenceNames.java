// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.preferences.editor;

import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.AbstractTextEditor;

/** <p>contains the preference names for the Haskell editor.</p>
  * 
  * @author Leif Frenzel
  */
public interface IEditorPreferenceNames {

  // Inherited constants
  String EDITOR_CURRENT_LINE 
    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE;
  String EDITOR_CURRENT_LINE_COLOR 
    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_CURRENT_LINE_COLOR;
  String EDITOR_LINE_NUMBER_RULER 
    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_LINE_NUMBER_RULER;
  String EDITOR_LINE_NUMBER_RULER_COLOR 
    = AbstractDecoratedTextEditorPreferenceConstants
        .EDITOR_LINE_NUMBER_RULER_COLOR;
  String EDITOR_PRINT_MARGIN 
    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN;
  String EDITOR_PRINT_MARGIN_COLOR 
    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLOR;
  String EDITOR_PRINT_MARGIN_COLUMN 
    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_PRINT_MARGIN_COLUMN;
  String EDITOR_OVERVIEW_RULER 
    = AbstractDecoratedTextEditorPreferenceConstants.EDITOR_OVERVIEW_RULER;
  String EDITOR_BACKGROUND_DEFAULT_COLOR 
    = AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND_SYSTEM_DEFAULT;
  String EDITOR_BACKGROUND_COLOR 
    = AbstractTextEditor.PREFERENCE_COLOR_BACKGROUND;
  String EDITOR_FOREGROUND_COLOR 
    = AbstractTextEditor.PREFERENCE_COLOR_FOREGROUND;
  String EDITOR_FOREGROUND_DEFAULT_COLOR 
   = AbstractTextEditor.PREFERENCE_COLOR_FOREGROUND_SYSTEM_DEFAULT;

  // Custom constants
  String EDITOR_TAB_WIDTH                  = "tabWidth";
  String EDITOR_MATCHING_BRACKETS          = "matchingBrackets";
  String EDITOR_MATCHING_BRACKETS_COLOR    = "matchingBracketsColor";
  String EDITOR_COMMENT_COLOR              = "commentColor";
  String EDITOR_COMMENT_BOLD               = "commentBold";
  String EDITOR_LITERATE_COMMENT_COLOR     = "literateCommentColor";
  String EDITOR_LITERATE_COMMENT_BOLD      = "literateCommentBold";
  String EDITOR_FUNCTION_COLOR             = "functionColor";
  String EDITOR_FUNCTION_BOLD              = "propertyBold";
  String EDITOR_KEYWORD_COLOR              = "keywordColor";
  String EDITOR_KEYWORD_BOLD               = "keywordBold";
  String EDITOR_STRING_COLOR               = "stringColor";
  String EDITOR_STRING_BOLD                = "stringBold";
  String EDITOR_DEFAULT_COLOR              = "defaultColor";
  String EDITOR_DEFAULT_BOLD               = "defaultBold";
  String EDITOR_SPACES_FOR_TABS            = "spacesForTabs";
  String EDITOR_CLOSE_STRINGS              = "closeStrings";
  String EDITOR_CLOSE_BRACKETS_AND_PARENS  = "closeBracketsAndParens";
  String EDITOR_CLOSE_BRACES               = "closeBraces";
  String EDITOR_SHOW_SELECTED_ELEMENT_ONLY = "showSelectedElementOnly";

  String CA_AUTOACTIVATION          = "contentAssistAutoActivation";
  String CA_AUTOACTIVATION_DELAY    = "contentAssistAutoActivationDelay";
  String CA_AUTOACTIVATION_TRIGGERS = "contentAssistAutoActivationTriggers";
  String CA_AUTOINSERT              = "contentAssistAutoInsert";
  String CA_ORDER_PROPOSALS         = "contentAssistOrderProposals";
  String CA_PROPOSALS_BACKGROUND    = "contentAssistProposalsBackground";
  String CA_PROPOSALS_FOREGROUND    = "contentAssistProposalsForeground";
}