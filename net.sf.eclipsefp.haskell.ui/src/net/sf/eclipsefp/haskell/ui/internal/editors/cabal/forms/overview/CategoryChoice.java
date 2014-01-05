/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;


/**
 * choice for project categories
 * @author JP Moresmau
 *
 */
public class CategoryChoice extends Choice<ProjectCategory> {

  @Override
  public ProjectCategory[] getValues() {
    return ProjectCategory.values();
  }

  @Override
  public boolean allowOther() {
    return true;
  }

  @Override
  public ProjectCategory fromCabalString( final String s ) {
    for( ProjectCategory l: getValues() ) {
      if( l.name().equals( s )) {
        return l;
      }
    }
    return null;
  }

  @Override
  public String toCabalString( final ProjectCategory o ) {
    return o.name();
  }

  @Override
  public ProjectCategory fromShownString( final String s ) {
    return fromCabalString( s );
  }

  @Override
  public String toShownString( final ProjectCategory o ) {
    return toCabalString( o );
  }



}
