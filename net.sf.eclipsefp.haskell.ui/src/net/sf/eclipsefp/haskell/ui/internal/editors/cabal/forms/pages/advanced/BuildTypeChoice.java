package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.pages.advanced;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;

public class BuildTypeChoice extends Choice<BuildType> {

  @Override
  public BuildType[] getValues() {
    return BuildType.values();
  }

  @Override
  public boolean allowOther() {
    return true;
  }

  @Override
  public BuildType fromCabalString( final String s ) {
    for( BuildType l: BuildType.values() ) {
      if( l.getCabalName().equals( s ) ) {
        return l;
      }
    }
    return null;
  }

  @Override
  public String toCabalString( final BuildType o ) {
    return o.getCabalName();
  }

  @Override
  public BuildType fromShownString( final String s ) {
    for( BuildType l: BuildType.values() ) {
      if( l.getShownName().equals( s ) ) {
        return l;
      }
    }
    return null;
  }

  @Override
  public String toShownString( final BuildType o ) {
    return o.getShownName();
  }

}
