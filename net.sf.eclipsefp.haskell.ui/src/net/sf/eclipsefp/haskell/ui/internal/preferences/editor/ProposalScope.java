/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;


/**
 * Scope of the search for content assist proposal
 * @author JP Moresmau
 *
 */
public enum ProposalScope {
  /**
   * only imported modules
   */
  IMPORTED,
  /**
   * all modules known to project
   */
  PROJECT,
  /**
   * all symbols in local package databases
   */
  ALL;

  /**
   * get the next proposal when cycling in content assist popup
   * @return the next scope
   */
  public ProposalScope next(){
    switch( this ) {
      case IMPORTED:
        return PROJECT;
      case PROJECT:
        return ALL;
      default:
        return IMPORTED;
    }
  }

  /**
   * get the user visible description of the scope
   * @return
   */
  public String getDescription(){
    switch( this ) {
      case IMPORTED:
        return UITexts.preferences_editor_contentass_scope_imported;
      case PROJECT:
        return  UITexts.preferences_editor_contentass_scope_project;
      case ALL:
        return UITexts.preferences_editor_contentass_scope_all;
    }
    return "";
  }
}
