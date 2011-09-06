/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;

/**
 * Possible versions of Cabal that may be set as minimal version required.
 * @author Alejandro Serrano
 *
 */
public class CabalVersionChoice extends Choice<String> {

  private static String[] values = new String[] { "1.2", "1.4", "1.6", "1.8", "1.10", "1.12" };

  @Override
  public String[] getValues() {
    return values;
  }

  @Override
  public boolean allowOther() {
    return true;
  }

  @Override
  public String fromCabalString( final String s ) {
    return s;
  }

  @Override
  public String toCabalString( final String o ) {
    return o;
  }

  @Override
  public String fromShownString( final String s ) {
    return s;
  }

  @Override
  public String toShownString( final String o ) {
    return o;
  }

}
