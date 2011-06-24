/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.ArrayList;


public abstract class Choice<T> {

  public abstract T[] getValues();

  public abstract boolean allowOther();

  public abstract T fromCabalString( String s );

  public abstract String toCabalString( T o );

  public String[] getAllCabalStrings() {
    ArrayList<String> strings = new ArrayList<String>();
    for (T value : getValues()) {
      strings.add( toCabalString(value) );
    }
    return strings.toArray( new String[strings.size()] );
  }

  public abstract T fromShownString( String s );

  public abstract String toShownString( T o );

  public String[] getAllShownStrings() {
    ArrayList<String> strings = new ArrayList<String>();
    for (T value : getValues()) {
      strings.add( toShownString(value) );
    }
    return strings.toArray( new String[strings.size()] );
  }
}
