// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.preferences;


/** <p>contains the names of preferences in the core plugin.</p>
  *
  * @author Leif Frenzel
  */
public interface ICorePreferenceNames {

  String SELECTED_COMPILER = "SELECTED_COMPILER"; //$NON-NLS-1$

  // Preference names for the Haskell implementations
  String HS_IMPLEMENTATIONS = "HS_IMPLEMENTATIONS"; //$NON-NLS-1$
  String SELECTED_HS_IMPLEMENTATION = "SELECTED_HS_IMPLEMENTATION"; //$NON-NLS-1$

  // Preference names for the Cabal implementations
  String CABAL_IMPLEMENTATIONS = "cabalImplementations"; //$NON-NLS-1$

  // new Haskell project
  String FOLDERS_SRC = "FOLDERS_SRC"; //$NON-NLS-1$
//  String FOLDERS_DOC = "FOLDERS_DOC"; //$NON-NLS-1$
  String FOLDERS_IN_NEW_PROJECT = "FOLDERS_IN_NEW_PROJECT"; //$NON-NLS-1$

  String DEBUG_BREAK_ON_ERROR="DEBUG_BREAK_ON_ERROR"; //$NON-NLS-1$
  String DEBUG_BREAK_ON_EXCEPTION="DEBUG_BREAK_ON_EXCEPTION"; //$NON-NLS-1$
  String DEBUG_PRINT_WITH_SHOW="DEBUG_PRINT_EVAL_WITH_SHOW"; //$NON-NLS-1$

  String RUN_COMMAND_HISTORY_MAX="RUN_COMMAND_HISTORY_MAX"; //$NON-NLS-1$


  String TEMPLATE_CABAL_LIBRARY="TEMPLATE_CABAL_LIBRARY"; //$NON-NLS-1$
  String TEMPLATE_CABAL_EXE="TEMPLATE_CABAL_EXE"; //$NON-NLS-1$
  String TEMPLATE_CABAL="TEMPLATE_CABAL"; //$NON-NLS-1$
  String TEMPLATE_CABAL_SETUP="TEMPLATE_CABAL_SETUP"; //$NON-NLS-1$

  String TEMPLATE_MODULE="TEMPLATE_MODULE"; //$NON-NLS-1$
//  String TEMPLATE_LIT="TEMPLATE_LIT"; //$NON-NLS-1$
//  String TEMPLATE_TEX="TEMPLATE_TEX"; //$NON-NLS-1$
  String TEMPLATE_MAIN="TEMPLATE_MAIN"; //$NON-NLS-1$
  String TEMPLATE_GTK="TEMPLATE_GTK"; //$NON-NLS-1$

}