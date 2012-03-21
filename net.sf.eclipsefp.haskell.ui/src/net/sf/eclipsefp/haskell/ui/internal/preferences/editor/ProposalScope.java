/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;


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

}
