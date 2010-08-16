// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.preferences;


/** <p>contains the names of preferences in the core plugin.</p>
  *
  * @author Leif Frenzel
  */
public interface ICorePreferenceNames {

  String SELECTED_COMPILER = "SELECTED_COMPILER"; //$NON-NLS-1$

  String HS_IMPLEMENTATIONS = "HS_IMPLEMENTATIONS"; //$NON-NLS-1$
  String SELECTED_HS_IMPLEMENTATION = "SELECTED_HS_IMPLEMENTATION"; //$NON-NLS-1$

  // new Haskell project
  String FOLDERS_SRC = "FOLDERS_SRC"; //$NON-NLS-1$
  String FOLDERS_DOC = "FOLDERS_DOC"; //$NON-NLS-1$
  String FOLDERS_IN_NEW_PROJECT = "FOLDERS_IN_NEW_PROJECT"; //$NON-NLS-1$

  String DEBUG_BREAK_ON_ERROR="DEBUG_BREAK_ON_ERROR"; //$NON-NLS-1$
  String DEBUG_BREAK_ON_EXCEPTION="DEBUG_BREAK_ON_EXCEPTION"; //$NON-NLS-1$
  String DEBUG_PRINT_WITH_SHOW="DEBUG_PRINT_EVAL_WITH_SHOW"; //$NON-NLS-1$

  // These preference names are no longer used:
  //String TARGET_BINARY = "TARGET_BINARY"; //$NON-NLS-1$
  //String FOLDERS_OUT = "FOLDERS_OUT"; //$NON-NLS-1$
}