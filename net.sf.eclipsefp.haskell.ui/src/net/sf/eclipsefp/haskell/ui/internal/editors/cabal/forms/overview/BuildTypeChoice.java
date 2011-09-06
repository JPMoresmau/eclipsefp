/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;

/**
 * Information about the types of build that can be used in a Cabal file.
 * @author Alejandro Serrano
 *
 */
public class BuildTypeChoice extends Choice<BuildType> {

  @Override
  public BuildType[] getValues() {
    return BuildType.values();
  }

  @Override
  public boolean allowOther() {
    return false;
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
