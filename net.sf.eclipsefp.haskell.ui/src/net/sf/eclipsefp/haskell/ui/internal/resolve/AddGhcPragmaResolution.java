/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.osgi.util.NLS;


/**
 * @author JP Moresmau
 *
 */
public class AddGhcPragmaResolution extends AddLanguagePragmaResolution {

  /**
   * @param pragma
   */
  public AddGhcPragmaResolution( final String pragma ) {
    super( pragma );

  }

  @Override
  protected String getPragmaStart(){
    return "{-# OPTIONS_GHC "; //$NON-NLS-1$
  }

  @Override
  protected String getSeparator(){
    return " "; //$NON-NLS-1$
  }

  @Override
  public String getLabel() {
    return NLS.bind( UITexts.resolve_addpragma, getPragma(),"OPTIONS_GHC" );//$NON-NLS-1$
  }

}
