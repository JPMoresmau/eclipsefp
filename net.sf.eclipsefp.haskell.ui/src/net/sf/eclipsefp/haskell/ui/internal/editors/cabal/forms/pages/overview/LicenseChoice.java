package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.pages.overview;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;

public class LicenseChoice extends Choice<License> {

  @Override
  public License[] getValues() {
    return License.values();
  }

  @Override
  public boolean allowOther() {
    return true;
  }

  @Override
  public License fromCabalString( final String s ) {
    for( License l: getValues() ) {
      if( l.getCabalName().equals( s ) ) {
        return l;
      }
    }
    return null;
  }

  @Override
  public String toCabalString( final License o ) {
    return o.getCabalName();
  }

  @Override
  public License fromShownString( final String s ) {
    for( License l: getValues() ) {
      if( l.getShownName().equals( s ) ) {
        return l;
      }
    }
    return null;
  }

  @Override
  public String toShownString( final License o ) {
    return o.getShownName();
  }

}
