package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;


public class CabalVersionChoice extends Choice<String> {

  private static String[] values = new String[] { "1.2", "1.4", "1.6", "1.8", "1.10" };

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
